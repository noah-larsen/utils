package consoleApplication

import java.io.File

import centralNamingsRepository.CentralNamingsRepository
import dataDictionary.{DataDictionary, ObjectAndFieldEntries, PhysicalNameObject}
import dataDictionary.enumerations.IngestionStages
import exceptions.DataHubException
import org.rogach.scallop.{ScallopConf, ScallopOption}
import consoleApplication.ConsoleRenamer.Languages.Language
import dataDictionary.enumerations.{Countries, SourceTypes}
import initialDataDictionary.InitialDataDictionary
import renaming.nameComparator.{CombinationNameComparator, SearchNameComparator, StringNameComparator}
import renaming.{ApprovedRenamings, NameSearch, Renaming, TargetName}
import us.USSourceSystems
import workDocument.{WorkDocument, WorkDocumentEntriesObject}

import scala.io.{Source, StdIn}
import scala.util.{Failure, Success, Try}

object Driver extends App {

  Try {

    class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
      val test: ScallopOption[Boolean] = toggle(default = Some(false), hidden = true)
      verify()
    }


    val conf = new Conf(args)
    val isTest = conf.test()


    val configPathname = "config.json"
    val testConfigPathname = "testConfig.json"
    val configuration = Configuration(Source.fromResource(if(isTest) testConfigPathname else configPathname)).get


    implicit val language: Language = configuration.language
    val lcSourceSystemToInitialDataDictionary = configuration.sourceSystemToInitialDataDictionaryId.map(x => (x._1.toLowerCase, InitialDataDictionary(x._2).get))
    val lcSourceSystemToDataDictionary = configuration.sourceSystemToDataDictionaryId.map(x => (x._1.toLowerCase, DataDictionary(x._2).get))
    val applicationId = configuration.applicationId
    val country = configuration.country
    val sourceType = SourceTypes.Table


    val centralNamingsRepository = CentralNamingsRepository().get
    val intermediateDataDictionary = DataDictionary(configuration.intermediateDataDictionaryId).get
    val workDocument = WorkDocument(configuration.workDocumentId).get


    main()


    def main(): Unit = {
      val commandInvocation = MainCommands.promptUntilParsed(leadWithNewline = false)
      commandInvocation.command match {
        case MainCommands.CreateFromInitialDataDictionary =>
          Try {
            val physicalNameObject = PhysicalNameObject(sourceType, applicationId, MainCommands.CreateFromInitialDataDictionary.sourceSystem(commandInvocation.arguments), MainCommands.CreateFromInitialDataDictionary.objectName(commandInvocation.arguments))
            val objectAndFieldEntries = ObjectAndFieldEntries(lcSourceSystemToInitialDataDictionary.getOrElse(physicalNameObject.sourceSystem.toLowerCase, throw DataHubException("Source not found")).objectNameToObjectAndFields.getOrElse(physicalNameObject
              .objectName, throw DataHubException("Object not found")))
            table(consoleRenamer(physicalNameObject, objectAndFieldEntries), objectAndFieldEntries)
          }.recover{case e: Exception => println(e.getMessage)}
          main()
        case MainCommands.LoadFromIntermediateDataDictionary =>
          Try {
            val physicalNameObject = PhysicalNameObject(sourceType, applicationId, MainCommands.LoadFromIntermediateDataDictionary.sourceSystem(commandInvocation.arguments), MainCommands.LoadFromIntermediateDataDictionary.objectName(commandInvocation
              .arguments))
            val objectAndFieldEntries = intermediateDataDictionary.objectAndFieldEntries(physicalNameObject.asString).get
            table(consoleRenamer(physicalNameObject, objectAndFieldEntries), objectAndFieldEntries)
          }.recover{case e: Exception => println(e.getMessage)}
          main()
        case MainCommands.WriteOnceToDataDictionary =>
          Try {
            val physicalNameObject = PhysicalNameObject(sourceType, applicationId, MainCommands.LoadFromIntermediateDataDictionary.sourceSystem(commandInvocation.arguments), MainCommands.LoadFromIntermediateDataDictionary.objectName(commandInvocation
              .arguments))
            workDocument.entriesObject(physicalNameObject).get.getOrElse(throw DataHubException("Object does not exist in work document")) match {
              case x if x.lowercaseSourceOrigin.isEmpty => throw DataHubException("Entries of approved object lack a consistent source origin.")
              case x if !x.allFieldsValidatedByLocalAndGlobalArchitecture => throw DataHubException("Work document entries exist for object that are unvalidated")
              case x if !lcSourceSystemToDataDictionary.contains(x.lowercaseSourceOrigin.get) => throw DataHubException("Cannot find dictionary for source system")
              case x if lcSourceSystemToDataDictionary(x.lowercaseSourceOrigin.get).fieldEntries(IngestionStages.Raw).get.exists(_.physicalNameObject.contains(x.table)) => throw DataHubException("Object already exists in data dictionary")
              case x =>
                x.mergeIfFromTextExtraction(intermediateDataDictionary, preserveRegistrationDatesThis = false, preserveRegistrationDatesThat = false)
                  .recoverWith { case e: DataHubException => Failure(e.copy(s"Could not merge with work data dictionary entries. ${e.message}")) }
                  .flatMap(y => lcSourceSystemToDataDictionary(x.lowercaseSourceOrigin.get).write(y.withRegistrationDates).map(_ => println("Wrote to data dictionary")))
                  .get
            }
          }.recover{case e: Exception => println(e.getMessage)}
          main()
        case MainCommands.Quit =>
      }
    }


    def consoleRenamer(physicalNameObject: PhysicalNameObject, objectAndFieldEntries: ObjectAndFieldEntries): ConsoleRenamer = {
      val dataDictionary = DataDictionary(configuration.sourceSystemToDataDictionaryId(physicalNameObject.sourceSystem)).get //todo handle key not found exception properly
      val approvedRenamings = ApprovedRenamings(dataDictionary).get
      val unapprovedRenamings = ApprovedRenamings(intermediateDataDictionary).get
      val targetNames = (centralNamingsRepository.targetNames ++ intermediateDataDictionary.targetNames(isApproved = false).get).groupBy(_.name).values.flatMap(x => if(x.exists(_.isApproved)) x.filter(_.isApproved) else x).toSeq
      val nameSearch = NameSearch(targetNames)
      val nTopHitsToGetPossiblyPositiveScores = 100
      val stringNameComparator = StringNameComparator(approvedRenamings.normalizedSubstringToMatchToNObservations)
      val searchNameComparator = SearchNameComparator(nameSearch, x => SearchNameComparator.joinWithSpaces(x.normalizedSubstrings ++ SearchNameComparator.splitByWhitespace(x.logicalName)), nTopHitsToGetPossiblyPositiveScores)
      val nameComparator = CombinationNameComparator(Map(stringNameComparator -> 1, searchNameComparator -> 1))
      ConsoleRenamer(Renaming(objectAndFieldEntries.rawFieldEntriesObject), targetNames, nameComparator, nameSearch, nTopHitsToGetPossiblyPositiveScores, approvedRenamings.originalToRenamedNameToNOccurences, unapprovedRenamings.originalToRenamedNameToNOccurences)
    }


    def table(consoleRenamer: ConsoleRenamer, objectAndFieldEntries: ObjectAndFieldEntries): Unit = {

      def saveWithRegistrationDates(renaming: Renaming): Try[ObjectAndFieldEntries] = {
        //todo specific exception message
        val withRegistrationDates = intermediateDataDictionary.objectAndFieldEntries(renaming.physicalNameObject.get).map(_ => objectAndFieldEntries.updateFieldEntriesIfFromTextExtraction(renaming)).getOrElse(objectAndFieldEntries
          .updateFieldEntriesIfFromTextExtraction(renaming).withRegistrationDates)
        intermediateDataDictionary.write(withRegistrationDates).map(_ => withRegistrationDates)
      }


      val commandInvocation = TableCommands.promptUntilParsed()
      commandInvocation.command match {
        case TableCommands.RenameFields => table(consoleRenamer.iterate, objectAndFieldEntries)
        case TableCommands.ViewRenamings => table(consoleRenamer.viewRenamings(), objectAndFieldEntries)
        case TableCommands.Save =>
          val withRegistrationDates = saveWithRegistrationDates(consoleRenamer.renaming)
          withRegistrationDates.failed.foreach(x => println(x.getMessage))
          Some(withRegistrationDates.getOrElse(objectAndFieldEntries)).foreach(x => table(consoleRenamer.copy(renaming = Renaming(x.rawFieldEntriesObject)), x))
        case TableCommands.WriteOnceToWorkDocument =>
          workDocument.writeOnce(WorkDocumentEntriesObject(consoleRenamer.renaming, centralNamingsRepository, preserveRegistrationDates = false).withRegistrationDates).recover{case e: Exception => println(e.getMessage)}
          table(consoleRenamer, objectAndFieldEntries)
        case TableCommands.GoBackWithoutSaving =>
      }

    }


  }.recover{case e: Exception => println(e.getMessage)}

}
