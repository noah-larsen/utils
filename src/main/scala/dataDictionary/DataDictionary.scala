package dataDictionary

import com.google.api.client.json.GenericJson
import consoleApplication.ConsoleRenamer.Languages.Language
import dataDictionary.enumerations.IngestionStages.{IngestionStage, Master, Raw}
import dataDictionary.enumerations.IngestionStages
import dataDictionary.field.{FieldEntriesObject, FieldEntry, FieldEntryReaderWriter}
import dataDictionary.`object`.{ObjectAndFieldEntries, ObjectEntry, ObjectEntryReaderWriter}
import exceptions.DataHubException
import googleSpreadsheets._
import renaming.TargetName

import scala.util.{Failure, Try}

case class DataDictionary(private val spreadsheet: GoogleSpreadsheet) {

  def containsEntriesFor(physicalNameObject: PhysicalNameObject): Try[Boolean] = {
    Try(objectEntry(Raw, physicalNameObject.asString).get.isDefined || objectEntry(Master, physicalNameObject.asString).get.isDefined || fieldEntriesObject(Raw, physicalNameObject.asString).get.isDefined || fieldEntriesObject(Master, physicalNameObject
      .asString).get.isDefined)
  }


  def fieldEntries(ingestionStage: IngestionStage): Try[Seq[FieldEntry]] = {
    spreadsheet.get(FieldEntryReaderWriter(ingestionStage))
  }


  def objectAndFieldEntries(physicalNameObject: String): Try[ObjectAndFieldEntries] = {
    Try {
      (objectEntry(Raw, physicalNameObject).get, objectEntry(Master, physicalNameObject).get, fieldEntriesObject(Raw, physicalNameObject).get, fieldEntriesObject(Master, physicalNameObject).get) match {
        case (Some(w), Some(x), Some(y), Some(z)) => ObjectAndFieldEntries(w, x, y, z)
        case _ => throw DataHubException("Object is missing entries from required tabs")
      }
    }
  }


  def write(objectAndFieldEntries: ObjectAndFieldEntries): Try[Seq[GenericJson]] = {
    //todo better error handling
    write(IngestionStages.Raw, objectAndFieldEntries.rawObjectEntry).map(Seq(_))
      .flatMap(x => write(IngestionStages.Master, objectAndFieldEntries.masterObjectEntry).map(x :+ _))
      .flatMap(x => write(IngestionStages.Raw, objectAndFieldEntries.rawFieldEntriesObject).map(x :+ _))
      .flatMap(x => write(IngestionStages.Master, objectAndFieldEntries.masterFieldEntriesObject).map(x :+ _))
  }


  def targetNames(isApproved: Boolean)(implicit language: Language): Try[Iterable[TargetName]] = {
    fieldEntries(IngestionStages.Raw).map(_.map(TargetName(_, isApproved)).groupBy(_.name).values.map(_.maxBy(_.description.length)))
  }


  private def fieldEntriesObject(ingestionStage: IngestionStage, physicalNameObject: String): Try[Option[FieldEntriesObject]] = {
    fieldEntries(ingestionStage).map(x => Some(FieldEntriesObject(x.filter(_.physicalNameObject.contains(physicalNameObject)))).filter(_.fieldEntries.nonEmpty))
  }


  private def objectEntry(ingestionStage: IngestionStage, physicalNameObject: String): Try[Option[ObjectEntry]] = {
    objectEntries(ingestionStage).map(_.filter(_.physicalNameObject.equalsIgnoreCase(physicalNameObject))).flatMap{
      case x if x.lengthCompare(1) <= 0 => Try(x.headOption)
      case x => Failure[Option[ObjectEntry]](DataHubException("Multiple spreadsheet object entries exist for the same object name"))
    }
  }


  private def objectEntries(ingestionStage: IngestionStage): Try[Seq[ObjectEntry]] = {
    spreadsheet.get(ObjectEntryReaderWriter(ingestionStage))
  }


  private def write(ingestionStage: IngestionStage, fieldEntriesObject: FieldEntriesObject): Try[GenericJson] = {
    write(fieldEntriesObject.fieldEntries, FieldEntryReaderWriter(ingestionStage), (x: FieldEntry) => x.physicalNameObject.exists(y => fieldEntriesObject.physicalNameObject.exists(_.equalsIgnoreCase(y))))
  }


  private def write(ingestionStage: IngestionStage, objectEntry: ObjectEntry): Try[GenericJson] = {
    write(Seq(objectEntry), ObjectEntryReaderWriter(ingestionStage), (x: ObjectEntry) => x.physicalNameObject.equalsIgnoreCase(objectEntry.physicalNameObject))
  }


  private def write[T <: Row](rows: Seq[T], rowReaderWriter: RowReaderWriter[T], isCorrespondingRow: T => Boolean): Try[GenericJson] = {
    def length(rangeInclusive: (Int, Int)): Int = rangeInclusive._2 - (rangeInclusive._1 - 1)
    val rangesInclusive = spreadsheet.rangesInclusive(rowReaderWriter, isCorrespondingRow)
    rangesInclusive.flatMap { rangesInclusive =>
      rangesInclusive.length match {
        case 0 => spreadsheet.append(rows, rowReaderWriter)
        case x if x == 1 && length(rangesInclusive.head) == rows.length => spreadsheet.update(rows, rowReaderWriter, Some(rangesInclusive.head._1), Some(rangesInclusive.head._2))
        case _ => spreadsheet.get(rowReaderWriter).flatMap(x => spreadsheet.update(x.filter(!isCorrespondingRow(_)) ++ rows, rowReaderWriter))
//        case x if x == 1 => Failure(DataHubException("Spreadsheet contains a different number of existing entries for the entity than the entries to overwrite with"))
//        case _ => Failure(DataHubException("Existing spreadsheet entries for the entity are not contiguous"))
      }
    }
  }

}

object DataDictionary {

  def apply(googleSpreadsheetId: String): Try[DataDictionary] = {
    GoogleSpreadsheet(googleSpreadsheetId).map(DataDictionary(_))
  }

}
