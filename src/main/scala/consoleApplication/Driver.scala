package consoleApplication

import java.io.File

import centralNamingsRepository.CentralNamingsRepository
import dataDictionary.{DataDictionary, PhysicalNameObject}
import dataDictionary.enumerations.IngestionStages
import exceptions.{DataHubException, InitialDataDictionaryNotFound, IntermediateDataDictionaryAlreadyContainsEntriesForObject, ObjectNotFoundInInitialDataDictionary}
import org.rogach.scallop.{ScallopConf, ScallopOption}
import consoleApplication.ConsoleRenamer.Languages.Language
import consoleApplication.MainCommands.{SourceSystem, TableName}
import dataDictionary.`object`.ObjectAndFieldEntries
import dataDictionary.enumerations.{Countries, SourceTypes}
import dataDictionary.field.FieldEntryReaderWriter.FieldEntryColumns
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


    def main(leadWithNewline: Boolean = true): Unit = {
      val commandInvocation = MainCommands.promptUntilParsed(leadWithNewline = leadWithNewline)
      commandInvocation.command match {
        case MainCommands.CreateFromInitial =>
          Try {
            val physicalNameObject = PhysicalNameObject(sourceType, applicationId, commandInvocation.value(SourceSystem), commandInvocation.value(TableName))
            if(intermediateDataDictionary.containsEntriesFor(physicalNameObject).get) throw IntermediateDataDictionaryAlreadyContainsEntriesForObject()
            val objectAndFieldEntries = fromInitialDataDictionary(physicalNameObject).get
            saveWithRegistrationDates(objectAndFieldEntries, intermediateDataDictionary)
            table(consoleRenamer(physicalNameObject, objectAndFieldEntries), objectAndFieldEntries, physicalNameObject)
          }.recover(displayError)
          main()
        case MainCommands.LoadFromIntermediate =>
          Try {
            val physicalNameObject = PhysicalNameObject(sourceType, applicationId, commandInvocation.value(SourceSystem), commandInvocation.value(TableName))
            val objectAndFieldEntries = intermediateDataDictionary.objectAndFieldEntries(physicalNameObject.asString).get
            table(consoleRenamer(physicalNameObject, objectAndFieldEntries), objectAndFieldEntries, physicalNameObject)
          }.recover(displayError)
          main()
        case MainCommands.WriteOnceToFinal =>
          Try {
            val physicalNameObject = PhysicalNameObject(sourceType, applicationId, commandInvocation.value(SourceSystem),commandInvocation.value(TableName))
            workDocument.entriesObject(physicalNameObject).get.getOrElse(throw DataHubException("Object does not exist in work document")) match {
              case x if x.lowercaseSourceOrigin.isEmpty => throw DataHubException("Entries of approved object lack a consistent source origin.")
//              case x if !x.allFieldsValidatedByLocalAndGlobalArchitecture => throw DataHubException("Work document entries exist for object that are unvalidated") //todo
              case x if !lcSourceSystemToDataDictionary.contains(x.lowercaseSourceOrigin.get) => throw DataHubException("Cannot find dictionary for source system")
              case x if lcSourceSystemToDataDictionary(x.lowercaseSourceOrigin.get).fieldEntries(IngestionStages.Raw).get.exists(_.physicalNameObject.contains(x.table)) => throw DataHubException("Object already exists in data dictionary")
              case x =>
                x.mergeIfFromTextExtraction(intermediateDataDictionary, preserveRegistrationDatesThis = false, preserveRegistrationDatesThat = false, Seq(FieldEntryColumns.PhysicalNameField))
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


    def table(consoleRenamer: ConsoleRenamer, objectAndFieldEntries: ObjectAndFieldEntries, physicalNameObject: PhysicalNameObject): Unit = {
      Try {
        val commandInvocation = TableCommands.promptUntilParsed()
        commandInvocation.command match {
          case TableCommands.RenameFields => table(consoleRenamer.iterate, objectAndFieldEntries, physicalNameObject)
          case TableCommands.ViewRenamings => table(consoleRenamer.viewRenamings(), objectAndFieldEntries, physicalNameObject)
          case TableCommands.SaveToIntermediate =>
            val withRegistrationDates = saveWithRegistrationDates(objectAndFieldEntries.updateFieldEntriesIfFromTextExtraction(consoleRenamer.renaming), intermediateDataDictionary)
            Some(withRegistrationDates.get).foreach(x => table(consoleRenamer.copy(renaming = Renaming(x.rawFieldEntriesObject)), x, physicalNameObject))
          case TableCommands.WriteOnceToWorkDocument =>
            workDocument.writeOnce(WorkDocumentEntriesObject(consoleRenamer.renaming, centralNamingsRepository, preserveRegistrationDates = false).withRegistrationDates).get
            table(consoleRenamer, objectAndFieldEntries, physicalNameObject)
          case TableCommands.UpdateFromInitial =>
            val fromInitial = fromInitialDataDictionary(physicalNameObject).get
            val updated = fromInitial.updateFieldEntriesIfFromTextExtraction(fromInitial.rawFieldEntriesObject.merge(objectAndFieldEntries.rawFieldEntriesObject, Seq(FieldEntryColumns.PhysicalNameField)).get)
            table(consoleRenamer.copy(renaming = Renaming(updated.rawFieldEntriesObject)), updated, physicalNameObject)
          case TableCommands.GoBackWithoutSaving =>
        }
      }.recover{
        case e: Exception =>
          displayError.apply(e)
          table(consoleRenamer, objectAndFieldEntries, physicalNameObject)
      }
    }


    def fromInitialDataDictionary(physicalNameObject: PhysicalNameObject): Try[ObjectAndFieldEntries] = Try {
      val lcSourceSystem = physicalNameObject.lcSourceSystem
      val lcObjectName = physicalNameObject.lcObjectName
      ObjectAndFieldEntries.fromTextExtraction(lcSourceSystemToInitialDataDictionary.getOrElse(lcSourceSystem, throw InitialDataDictionaryNotFound(lcSourceSystem)).lcObjectNameToObjectAndFields.map(_.getOrElse(lcObjectName, throw
        ObjectNotFoundInInitialDataDictionary(lcObjectName))).get, generatedFields)
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
