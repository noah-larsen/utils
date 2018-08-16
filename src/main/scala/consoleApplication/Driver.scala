package consoleApplication

import java.io.File

import centralNamingsRepository.CentralNamingsRepository
import dataDictionary.{DataDictionary, PhysicalNameObject}
import dataDictionary.enumerations.IngestionStages
import exceptions.{DataHubException, IntermediateDataDictionaryAlreadyContainsEntriesForObject}
import org.rogach.scallop.{ScallopConf, ScallopOption}
import consoleApplication.ConsoleRenamer.Languages.Language
import dataDictionary.`object`.ObjectAndFieldEntries
import dataDictionary.enumerations.{Countries, SourceTypes}
import initialDataDictionary.InitialDataDictionary
import renaming.nameComparator.{CombinationNameComparator, SearchNameComparator, StringNameComparator}
import renaming.{ApprovedRenamings, NameSearch, Renaming, TargetName}
import us.USSourceSystems
import utils.Retry
import workDocument.{WorkDocument, WorkDocumentEntriesObject}

import scala.io.{Source, StdIn}
import scala.util.{Failure, Success, Try}

object Driver extends App {

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val test: ScallopOption[Boolean] = toggle(default = Some(false), hidden = true)
    verify()
  }


  val conf = new Conf(args)
  implicit val isTest: Boolean = conf.test()


  Try {

    val configPathname = "config.json"
    val testConfigPathname = "testConfig.json"
    val configuration = Configuration(Source.fromResource(if(isTest) testConfigPathname else configPathname)).get


    implicit val language: Language = configuration.language
    //todo error handling
    val applicationId = configuration.applicationId
    val country = configuration.country
    val generatedFields = configuration.generatedFields
    val lcSourceSystemToInitialDataDictionary = configuration.lcSourceSystemToInitialDataDictionaryId.filter(_._2.trim.nonEmpty).map(x => (x._1.toLowerCase, InitialDataDictionary(x._2).get))
    val lcSourceSystemToDataDictionary = configuration.lcSourceSystemToDataDictionaryId.filter(_._2.trim.nonEmpty).map(x => (x._1.toLowerCase, DataDictionary(x._2).get))
    val sourceType = SourceTypes.Table //todo


    //todo make spreadsheet id a part of config
    val centralNamingsRepository = Retry(CentralNamingsRepository(), uponRetry = {case _ => println("Retrying...")}).get
    val intermediateDataDictionary = DataDictionary(configuration.intermediateDataDictionaryId).get
    val workDocument = WorkDocument(configuration.workDocumentId).get


    main(false)


    def main(leadWithNewLine: Boolean = true): Unit = {
      val commandInvocation = MainCommands.promptUntilParsed(leadWithNewLine = leadWithNewLine)
      commandInvocation.command match {
        case MainCommands.CreateFromInitial =>
          Try {
            val physicalNameObject = PhysicalNameObject(sourceType, applicationId, MainCommands.CreateFromInitial.sourceSystem(commandInvocation.arguments), MainCommands.CreateFromInitial.objectName(commandInvocation.arguments))
            if(intermediateDataDictionary.containsEntriesFor(physicalNameObject).get) throw IntermediateDataDictionaryAlreadyContainsEntriesForObject
            val objectAndFieldEntries = ObjectAndFieldEntries.fromTextExtraction(lcSourceSystemToInitialDataDictionary.getOrElse(physicalNameObject.sourceSystem.toLowerCase, throw DataHubException("Source not found")).lcObjectNameToObjectAndFields.map(_
              .getOrElse(physicalNameObject.objectName.toLowerCase, throw DataHubException("Object not found"))).get, generatedFields)
            saveWithRegistrationDates(objectAndFieldEntries, intermediateDataDictionary)
            table(consoleRenamer(physicalNameObject, objectAndFieldEntries), objectAndFieldEntries)
          }.recover(displayError)
          main()
        case MainCommands.LoadFromIntermediate =>
          Try {
            val physicalNameObject = PhysicalNameObject(sourceType, applicationId, MainCommands.LoadFromIntermediate.sourceSystem(commandInvocation.arguments), MainCommands.LoadFromIntermediate.objectName(commandInvocation
              .arguments))
            val objectAndFieldEntries = intermediateDataDictionary.objectAndFieldEntries(physicalNameObject.asString).get
            table(consoleRenamer(physicalNameObject, objectAndFieldEntries), objectAndFieldEntries)
          }.recover(displayError)
          main()
        case MainCommands.WriteOnceToFinal =>
          Try {
            val physicalNameObject = PhysicalNameObject(sourceType, applicationId, MainCommands.LoadFromIntermediate.sourceSystem(commandInvocation.arguments), MainCommands.LoadFromIntermediate.objectName(commandInvocation
              .arguments))
            workDocument.entriesObject(physicalNameObject).get.getOrElse(throw DataHubException("Object does not exist in work document")) match {
              case x if x.lowercaseSourceOrigin.isEmpty => throw DataHubException("Entries of approved object lack a consistent source origin.")

              //todo
//              case x if !x.allFieldsValidatedByLocalAndGlobalArchitecture => throw DataHubException("Work document entries exist for object that are unvalidated")

              case x if !lcSourceSystemToDataDictionary.contains(x.lowercaseSourceOrigin.get) => throw DataHubException("Cannot find dictionary for source system")
              case x if lcSourceSystemToDataDictionary(x.lowercaseSourceOrigin.get).fieldEntries(IngestionStages.Raw).get.exists(_.physicalNameObject.contains(x.table)) => throw DataHubException("Object already exists in data dictionary")
              case x =>
                x.mergeIfFromTextExtraction(intermediateDataDictionary, preserveRegistrationDatesThis = false, preserveRegistrationDatesThat = false)
                  .recoverWith { case e: DataHubException => Failure(DataHubException(s"Could not merge with work data dictionary entries. ${e.getMessage}")) }
                  .flatMap(y => saveWithRegistrationDates(y, lcSourceSystemToDataDictionary(x.lowercaseSourceOrigin.get)).map(_ => println("Wrote to data dictionary")))
                  .get
            }
          }.recover(displayError)
          main()
        case MainCommands.Quit =>
      }
    }


    def consoleRenamer(physicalNameObject: PhysicalNameObject, objectAndFieldEntries: ObjectAndFieldEntries): ConsoleRenamer = {
      val dataDictionary = DataDictionary(configuration.lcSourceSystemToDataDictionaryId(physicalNameObject.sourceSystem.toLowerCase)).get //todo handle key not found exception properly
      val approvedRenamings = ApprovedRenamings(dataDictionary).get
      val unapprovedRenamings = ApprovedRenamings(intermediateDataDictionary).get
      val targetNames = (centralNamingsRepository.targetNames ++ intermediateDataDictionary.targetNames(isApproved = false).get).groupBy(_.name).values.flatMap(x => if(x.exists(_.isApproved)) x.filter(_.isApproved) else x).toSeq
      val nameSearch = NameSearch(targetNames)
      val nTopHitsToGetPossiblyPositiveScores = 100
      val stringNameComparator = StringNameComparator(approvedRenamings.normalizedSubstringToMatchToNObservations)
      val searchNameComparator = SearchNameComparator(nameSearch, x => SearchNameComparator.joinWithSpaces(x.normalizedSubstrings ++ SearchNameComparator.splitByWhitespace(x.logicalName)), nTopHitsToGetPossiblyPositiveScores)
      val nameComparator = CombinationNameComparator(Map(stringNameComparator -> 1, searchNameComparator -> 1))
      ConsoleRenamer(Renaming(objectAndFieldEntries.rawFieldEntriesObject), targetNames, nameComparator, nameSearch, nTopHitsToGetPossiblyPositiveScores, approvedRenamings.originalToRenamedNameToNOccurences, unapprovedRenamings
        .originalToRenamedNameToNOccurences)
    }


    def table(consoleRenamer: ConsoleRenamer, objectAndFieldEntries: ObjectAndFieldEntries): Unit = {

      val commandInvocation = TableCommands.promptUntilParsed()
      commandInvocation.command match {
        case TableCommands.RenameFields => table(consoleRenamer.iterate, objectAndFieldEntries)
        case TableCommands.ViewRenamings => table(consoleRenamer.viewRenamings(), objectAndFieldEntries)
        case TableCommands.SaveToIntermediate =>
          val withRegistrationDates = saveWithRegistrationDates(objectAndFieldEntries.updateFieldEntriesIfFromTextExtraction(consoleRenamer.renaming), intermediateDataDictionary)
          withRegistrationDates.recover(displayError)
          Some(withRegistrationDates.getOrElse(objectAndFieldEntries)).foreach(x => table(consoleRenamer.copy(renaming = Renaming(x.rawFieldEntriesObject)), x))
        case TableCommands.WriteOnceToWorkDocument =>
          workDocument.writeOnce(WorkDocumentEntriesObject(consoleRenamer.renaming, centralNamingsRepository, preserveRegistrationDates = false).withRegistrationDates).recover(displayError)
          table(consoleRenamer, objectAndFieldEntries)
        case TableCommands.GoBackWithoutSaving =>
      }

    }


    def saveWithRegistrationDates(objectAndFieldEntries: ObjectAndFieldEntries, toDataDictionary: DataDictionary): Try[ObjectAndFieldEntries] = {
      //todo specific exception message
      val withRegistrationDates = toDataDictionary.objectAndFieldEntries(objectAndFieldEntries.rawObjectEntry.physicalNameObject).map(_ => objectAndFieldEntries).getOrElse(objectAndFieldEntries.withRegistrationDates)
      toDataDictionary.write(withRegistrationDates).map(_ => withRegistrationDates)
    }


    def displayError(implicit isTest: Boolean): PartialFunction[Throwable, Unit] = {
      PartialFunction {
        case e: DataHubException => println(e.getMessage)
        case e: Exception if isTest => e.printStackTrace()
        case e: Exception => println(e.getMessage)
      }
    }

  }.recover{case e: Exception => if(isTest) e.printStackTrace() else println(e.getMessage)}

}
