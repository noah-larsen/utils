package renaming.consoleApplication

import java.io.File

import centralNamingsRepository.CentralNamingsRepository
import com.typesafe.config.ConfigFactory
import dataDictionary.{DataDictionary, PhysicalNameObject}
import dataDictionary.FieldEntry.IngestionStages
import dataDictionary.ObjectRow.Countries
import dataDictionary.PhysicalNameObject.SourceTypes
import general.DataHubException
import org.rogach.scallop.{ScallopConf, ScallopOption}
import renaming.consoleApplication.ConsoleRenamer.Languages
import renaming.{ApprovedRenamings, NameComparator, Renaming}
import us.{AlnovaTableLayouts, USSources}
import workDocument.{WorkDocument, WorkDocumentEntriesObject}

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object Driver extends App {

  Try {

    val configPathname = "config.json" //todo
    val configuration = Configuration(configPathname).get


    class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
      verify()
    }


    val conf = new Conf(args)
    implicit val language: Languages.Value = configuration.language
    val centralNamingsRepository = CentralNamingsRepository().get
    val workDataDictionary = DataDictionary(configuration.workDataDictionaryId).get
    val workDocument = WorkDocument(configuration.workDocumentId).get
    val lowercaseSourceOriginToDataDictionary = configuration.sourceSystemToDataDictionaryId.map(x => (x._1.toLowerCase, DataDictionary(x._2).get))
    val applicationId = configuration.applicationId
    val country = configuration.country


    main()


    def main(): Unit = {
      val commandInvocation = MainCommands.promptUntilParsed()
      commandInvocation.command match {
        case MainCommands.Load =>
          Try {
            val sourceSystem = MainCommands.Load.sourceSystem(commandInvocation.arguments)
            val tableName = MainCommands.Load.tableName(commandInvocation.arguments)
            val dataDictionary = DataDictionary(configuration.sourceSystemToDataDictionaryId(sourceSystem)).get
            val approvedRenamings = ApprovedRenamings(dataDictionary)
            val nameComparator = NameComparator(approvedRenamings.get.normalizedSubstringToMatchToNObservations)
            val sourceType = SourceTypes.Table
            val physicalNameObject = PhysicalNameObject(sourceType, applicationId, sourceSystem, tableName)
            val renaming = workDataDictionary.fieldEntriesObject(IngestionStages.Raw, physicalNameObject.string).flatMap(_.map(Try(_)).orElse(Some(Unit).filter(_ => country == Countries.UnitedStates).flatMap(_ => USSources
              .fieldEntriesObject(physicalNameObject))).getOrElse(Failure(DataHubException("Source not found"))).map(_.withCountry(country))).map(Renaming(_))
            table(ConsoleRenamer(renaming.get, centralNamingsRepository.globalNamings, nameComparator))
          }.recover { case e => println(e.getMessage) }
          main()
        case MainCommands.WriteUnwrittenApprovedObjectsToDataDictionaries =>
          workDocument.approvedEntriesObjects match {
            case Success(approvedEntriesObjects) => approvedEntriesObjects.foreach {
              case x if x.lowercaseSourceOrigin.isEmpty => println(s"${x.table}: Entries of approved object lack a consistent source origin.")
              case x if !lowercaseSourceOriginToDataDictionary.contains(x.lowercaseSourceOrigin.get) => println(s"${x.table}: Cannot find dictionary for source origin ${x.lowercaseSourceOrigin.get}.")
              case x if lowercaseSourceOriginToDataDictionary(x.lowercaseSourceOrigin.get).fieldEntries(IngestionStages.Raw).get.exists(_.physicalNameObject.contains(x.table)) => //todo error handling on get in condition
              case x =>
                println(
                  x.merge(workDataDictionary, preserveRegistrationDatesThis = false, preserveRegistrationDatesThat = false)
                    .recoverWith { case e: DataHubException => Failure(e.copy(s"${x.table}: Could not merge with work data dictionary entries. ${e.message}")) }
                    .flatMap(y => lowercaseSourceOriginToDataDictionary(x.lowercaseSourceOrigin.get).write(IngestionStages.Raw, y.withRegistrationDates).map(_ => s"${x.table}: Wrote to data dictionary."))
                    .recover { case e: DataHubException => e.message }
                    .get
                )
            }
            case Failure(e) => println(e.getMessage)
          }

          main()
        case MainCommands.Quit =>
      }
    }


    def table(consoleRenamer: ConsoleRenamer): Unit = {

      def saveWithRegistrationDates(renaming: Renaming) = {
        workDataDictionary.write(IngestionStages.Raw, workDataDictionary.fieldEntriesObject(IngestionStages.Raw, renaming.physicalNameObject.get).map(_ => renaming).getOrElse(renaming.withRegistrationDates))
      }


      val commandInvocation = TableCommands.promptUntilParsed()
      commandInvocation.command match {
        case TableCommands.IterateUnnamedFields => table(consoleRenamer.iterate)
        case TableCommands.ViewRenamings =>
          consoleRenamer.viewRenamings()
          table(consoleRenamer)
        case TableCommands.Save =>
          saveWithRegistrationDates(consoleRenamer.renaming).recover{case e => println(e.getMessage)}
          table(consoleRenamer)
        case TableCommands.WriteOnceToWorkDocumentAndSave =>
          saveWithRegistrationDates(consoleRenamer.renaming)
            .flatMap(_ => workDocument.writeOnce(WorkDocumentEntriesObject(consoleRenamer.renaming, centralNamingsRepository, preserveRegistrationDates = false).withRegistrationDates))
            .recover{case e => println(e.getMessage)}
          table(consoleRenamer)
        case TableCommands.GoBackWithoutSaving =>
      }

    }


  }.recover{case e => println(e.getMessage)}

}
