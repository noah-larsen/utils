package consoleApplication

import java.io.File

import centralNamingsRepository.CentralNamingsRepository
import com.google.api.client.json.GenericJson
import com.typesafe.config.ConfigFactory
import dataDictionary.{DataDictionary, FieldEntriesObject, PhysicalNameObject}
import dataDictionary.FieldEntry.IngestionStages
import dataDictionary.ObjectRow.Countries
import dataDictionary.PhysicalNameObject.SourceTypes
import general.DataHubException
import org.rogach.scallop.{ScallopConf, ScallopOption}
import consoleApplication.ConsoleRenamer.Languages
import consoleApplication.ConsoleRenamer.Languages.Language
import renaming.NameSearch.Fields
import renaming.nameComparator.{CombinationNameComparator, SearchNameComparator, StringNameComparator}
import renaming.{ApprovedName, ApprovedRenamings, NameSearch, Renaming}
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
    val lowercaseSourceOriginToDataDictionary = configuration.sourceSystemToDataDictionaryId.map(x => (x._1.toLowerCase, DataDictionary(x._2).get))
    val applicationId = configuration.applicationId
    val country = configuration.country
    val sourceType = SourceTypes.Table


    val centralNamingsRepository = CentralNamingsRepository().get
    val workDataDictionary = DataDictionary(configuration.workDataDictionaryId).get
    val workDocument = WorkDocument(configuration.workDocumentId).get


    main()


    def main(): Unit = {
      val commandInvocation = MainCommands.promptUntilParsed(leadWithNewlineInitial = false)
      commandInvocation.command match {
        case MainCommands.Load =>
          Try {
            val physicalNameObject = PhysicalNameObject(sourceType, applicationId, MainCommands.Load.sourceSystem(commandInvocation.arguments), MainCommands.Load.dataName(commandInvocation.arguments))
            val dataDictionary = DataDictionary(configuration.sourceSystemToDataDictionaryId(physicalNameObject.sourceSystem)).get //todo handle key not found exception properly
            val approvedRenamings = ApprovedRenamings(dataDictionary).get
            val approvedNames = centralNamingsRepository.globalNamings.map(ApprovedName(_))

            //todo
            val nameSearch = NameSearch(approvedNames)
            val nTopHitsToGetPossiblyPositiveScores = 100
            val stringNameComparator = StringNameComparator(approvedRenamings.normalizedSubstringToMatchToNObservations)
            val searchNameComparator = SearchNameComparator(nameSearch, x => SearchNameComparator.joinWithSpaces(x.normalizedSubstrings ++ SearchNameComparator.splitByWhitespace(x.logicalName)), nTopHitsToGetPossiblyPositiveScores)
            val nameComparator = CombinationNameComparator(Map(stringNameComparator -> 1, searchNameComparator -> 1))

            val renaming = workDataDictionary.fieldEntriesObject(IngestionStages.Raw, physicalNameObject.string).flatMap(_.map(Try(_)).orElse(Some(Unit).filter(_ => country == Countries.UnitedStates).flatMap(_ => USSourceSystems
              .fieldEntriesObject(physicalNameObject))).getOrElse(Failure(DataHubException("Source not found"))).map(_.withCountry(country))).map(Renaming(_))
            table(ConsoleRenamer(renaming.get, approvedNames, nameComparator, nameSearch, nTopHitsToGetPossiblyPositiveScores, approvedRenamings.originalToRenamedNameToNOccurences))
          }.recover{case e: Exception => println(e.getMessage)}
          main()
        case MainCommands.WriteOnceToDataDictionary =>
          val physicalNameObject = PhysicalNameObject(sourceType, applicationId, MainCommands.Load.sourceSystem(commandInvocation.arguments), MainCommands.Load.dataName(commandInvocation.arguments))
          println(workDocument.entriesObject(physicalNameObject).flatMap(_.map(Try(_)).getOrElse(Failure(DataHubException("Object does not exist in work document")))).map{
            case x if x.lowercaseSourceOrigin.isEmpty => "Entries of approved object lack a consistent source origin."
            case x if !x.allFieldsValidatedByLocalAndGlobalArchitecture => "Work document entries exist for object that are unvalidated"
            case x if !lowercaseSourceOriginToDataDictionary.contains(x.lowercaseSourceOrigin.get) => "Cannot find dictionary for source system"
            case x if lowercaseSourceOriginToDataDictionary(x.lowercaseSourceOrigin.get).fieldEntries(IngestionStages.Raw).get.exists(_.physicalNameObject.contains(x.table)) => "Object already exists in data dictionary"
            case x =>
                x.merge(workDataDictionary, preserveRegistrationDatesThis = false, preserveRegistrationDatesThat = false)
                  .recoverWith { case e: DataHubException => Failure(e.copy(s"Could not merge with work data dictionary entries. ${e.message}")) }
                  .flatMap(y => lowercaseSourceOriginToDataDictionary(x.lowercaseSourceOrigin.get).write(IngestionStages.Raw, y.withRegistrationDates).map(_ => "Wrote to data dictionary"))
                  .recover { case e: DataHubException => e.message }
                  .get
          }.recover{case e: Exception => e.getMessage}.get)
          main()
        case MainCommands.Quit =>
      }
    }


    def table(consoleRenamer: ConsoleRenamer): Unit = {

      def saveWithRegistrationDates(renaming: Renaming): Try[Renaming] = {
        //todo specific exception message
        val withRegistrationDates = workDataDictionary.fieldEntriesObject(IngestionStages.Raw, renaming.physicalNameObject.get).map(_.map(_ => renaming).getOrElse(Renaming(renaming.withRegistrationDates)))
        withRegistrationDates.flatMap(workDataDictionary.write(IngestionStages.Raw, _)).map(_ => withRegistrationDates.get)
      }


      val commandInvocation = TableCommands.promptUntilParsed()
      commandInvocation.command match {
        case TableCommands.IterateUnnamedFields => table(consoleRenamer.iterate)
        case TableCommands.ViewRenamings =>
          consoleRenamer.viewRenamings()
          table(consoleRenamer)
        case TableCommands.Save =>
          val withRegistrationDates = saveWithRegistrationDates(consoleRenamer.renaming)
          withRegistrationDates.failed.foreach(x => println(x.getMessage))
          table(withRegistrationDates.map(x => consoleRenamer.copy(renaming = x)).getOrElse(consoleRenamer))
        case TableCommands.WriteOnceToWorkDocument =>
          workDocument.writeOnce(WorkDocumentEntriesObject(consoleRenamer.renaming, centralNamingsRepository, preserveRegistrationDates = false).withRegistrationDates).recover{case e: Exception => println(e.getMessage)}
          table(consoleRenamer)
        case TableCommands.GoBackWithoutSaving =>
      }

    }


  }.recover{case e: Exception => println(e.getMessage)}

}
