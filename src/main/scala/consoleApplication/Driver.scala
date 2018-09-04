package consoleApplication

import java.io.File
import java.time.LocalDate

import centralNamingsRepository.CentralNamingsRepository
import dataDictionary.{DataDictionary, PhysicalNameObject}
import dataDictionary.enumerations.IngestionStages
import exceptions._
import org.rogach.scallop.{ScallopConf, ScallopOption}
import consoleApplication.ConsoleRenamer.Languages.Language
import consoleApplication.MainCommands.{SourceSystem, TableName}
import dataDictionary.`object`.ObjectAndFieldEntries
import dataDictionary.enumerations.{Countries, SourceTypes}
import dataDictionary.field.FieldEntryColumns
import dataDictionary.field.FieldEntryColumns.PhysicalNameField
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
    val primaryDateFieldTemplate = configuration.primaryDateFieldTemplate
    val lcSourceSystemToInitialDataDictionary = configuration.lcSourceSystemToInitialDataDictionaryId.filter(_._2.trim.nonEmpty).map(x => (x._1.toLowerCase, InitialDataDictionary(x._2).get))
    val lcSourceSystemToDataDictionary = configuration.lcSourceSystemToDataDictionaryId.filter(_._2.trim.nonEmpty).map(x => (x._1.toLowerCase, DataDictionary(x._2).get))
    val sourceType = SourceTypes.Table //todo


    //todo make spreadsheet id a part of config
    val centralNamingsRepository = Retry(CentralNamingsRepository(), uponRetry = {case _ => println("Retrying...")}).get
    val intermediateDataDictionary = DataDictionary(configuration.intermediateDataDictionaryId).get
    val workDocument = WorkDocument(configuration.workDocumentId).get


    main(false)


    def main(leadWithNewline: Boolean = true): Unit = {
      Try {
        val commandInvocation = MainCommands.promptUntilParsed(leadWithNewline = leadWithNewline)
        commandInvocation.command match {
          case MainCommands.Load =>
            val physicalNameObject = PhysicalNameObject(sourceType, applicationId, commandInvocation.value(SourceSystem), commandInvocation.value(TableName))
            val objectAndFieldEntries = load(physicalNameObject).get
            table(consoleRenamer(physicalNameObject, objectAndFieldEntries), objectAndFieldEntries, physicalNameObject)
          case MainCommands.WriteToFinal =>
            writeToFinal(PhysicalNameObject(sourceType, applicationId, commandInvocation.value(SourceSystem), commandInvocation.value(TableName)))
          case MainCommands.UpdateAllInFinal =>
            val lcSourceSystem = commandInvocation.value(SourceSystem).toString.toLowerCase
            val finalDataDictionary = lcSourceSystemToDataDictionary.getOrElse(lcSourceSystem, throw FinalDataDictionaryNotFound(lcSourceSystem))
            finalDataDictionary.lcPhysicalNamesObjects.get.foreach{ physicalNameObject =>
              //todo look into how physical name objects are reconstructed
              writeToFinal(PhysicalNameObject(physicalNameObject, Set(lcSourceSystem)).get).get
            }
          case MainCommands.Quit => System.exit(0)
        }
      }.recover(displayError)
      main()
    }


    def load(physicalNameObject: PhysicalNameObject): Try[ObjectAndFieldEntries] = Try {
      val objectAndFieldEntriesInitial = fromInitialDataDictionary(physicalNameObject).get
      val intermediateRawFieldEntriesObject = intermediateDataDictionary.rawFieldEntriesObject(physicalNameObject.asString).get
      val workDocumentEntriesObject = workDocument.entriesObject(physicalNameObject).get
      val rawFEOInitialIntermediate = intermediateRawFieldEntriesObject.map(objectAndFieldEntriesInitial.rawFieldEntriesObject.merge(_, Seq(PhysicalNameField)).withoutRegistrationDates).getOrElse(objectAndFieldEntriesInitial.rawFieldEntriesObject)
      val rawFEOInitialIntermediateWorkDocument = workDocumentEntriesObject.map(x => rawFEOInitialIntermediate.merge(x.toRawFieldEntriesObject(false), Seq(PhysicalNameField))).getOrElse(rawFEOInitialIntermediate)
      objectAndFieldEntriesInitial.updateFieldEntriesIfFromTextExtraction(rawFEOInitialIntermediateWorkDocument)
    }

    def writeToFinal(physicalNameObject: PhysicalNameObject): Try[Unit] = Try {

      def loadForFinal(physicalNameObject: PhysicalNameObject): Try[ObjectAndFieldEntries] = Try {
        val objectAndFieldEntriesInitial = fromInitialDataDictionary(physicalNameObject).get
        val rawFEOInitial = objectAndFieldEntriesInitial.rawFieldEntriesObject
        val workDocumentEntriesObject = workDocument.entriesObject(physicalNameObject).get
        if(workDocumentEntriesObject.isEmpty) throw WorkDocumentEntriesForObjectDoNotExist(physicalNameObject.asLcString)
        val rawFEOInitialWorkDocument = workDocumentEntriesObject.map(x => rawFEOInitial.merge(x.toRawFieldEntriesObject(false), Seq(PhysicalNameField))).getOrElse(rawFEOInitial)
        val objectAndFieldEntriesInitialWorkDocument = objectAndFieldEntriesInitial.updateFieldEntriesIfFromTextExtraction(rawFEOInitialWorkDocument)
        if(objectAndFieldEntriesInitialWorkDocument.containsDuplicateFieldNames) throw ObjectContainsDuplicateFieldNames(physicalNameObject.asLcString)
        objectAndFieldEntriesInitialWorkDocument
      }


      val objectAndFieldEntries = loadForFinal(physicalNameObject).get
      saveWithRegistrationDates(objectAndFieldEntries, lcSourceSystemToDataDictionary.getOrElse(physicalNameObject.lcSourceSystem, throw FinalDataDictionaryNotFound(physicalNameObject.lcSourceSystem))).get

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
        ObjectNotFoundInInitialDataDictionary(lcObjectName))).get, generatedFields, primaryDateFieldTemplate)
    }


    def saveWithRegistrationDates(objectAndFieldEntries: ObjectAndFieldEntries, toDataDictionary: DataDictionary): Try[ObjectAndFieldEntries] = {
      //todo specific exception message
      val withRegistrationDates = toDataDictionary.objectAndFieldEntries(objectAndFieldEntries.rawObjectEntry.physicalNameObject).map(x => objectAndFieldEntries.withRegistrationDate(x.maxRegistrationDate.getOrElse(LocalDate.now()))).getOrElse(
        objectAndFieldEntries.withRegistrationDates)
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
