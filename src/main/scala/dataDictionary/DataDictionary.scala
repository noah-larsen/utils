package dataDictionary

import com.google.api.client.json.GenericJson
import dataDictionary.FieldEntry.IngestionStages.IngestionStage
import dataDictionary.FieldEntry._
import exceptions.DataHubException
import googleSpreadsheets._

import scala.util.{Failure, Try}

case class DataDictionary(private val spreadsheet: GoogleSpreadsheet) {

  def fieldEntries(ingestionStage: IngestionStage): Try[Seq[FieldEntry]] = {
    spreadsheet.get[FieldEntry](FieldEntryReaderWriter(ingestionStage))
  }


  def fieldEntriesObject(ingestionStage: IngestionStage, physicalNameObject: String): Try[Option[FieldEntriesObject]] = {
    fieldEntries(ingestionStage).map(x => Some(FieldEntriesObject(x.filter(_.physicalNameObject.contains(physicalNameObject)))).filter(_.fieldEntries.nonEmpty))
  }


  def write(ingestionStage: IngestionStage, fieldEntriesObject: FieldEntriesObject): Try[GenericJson] = {
    def length(rangeInclusive: (Int, Int)): Int = rangeInclusive._2 - (rangeInclusive._1 - 1)
    val dataReaderWriter = FieldEntryReaderWriter(ingestionStage)
    val rangesInclusive = spreadsheet.rangesInclusive(dataReaderWriter, (x: FieldEntry) => x.physicalNameObject.exists(y => fieldEntriesObject.physicalNameObject.exists(_.equalsIgnoreCase(y))))
    rangesInclusive.flatMap { rangesInclusive =>
      rangesInclusive.length match {
        case 0 => spreadsheet.append(fieldEntriesObject.fieldEntries, dataReaderWriter)
        case x if x == 1 && length(rangesInclusive.head) == fieldEntriesObject.nEntries => spreadsheet.update(fieldEntriesObject.fieldEntries, dataReaderWriter, Some(rangesInclusive.head._1), Some(rangesInclusive.head._2))
        case x if x == 1 => Failure(DataHubException("Spreadsheet contains a different number of field entries for the object"))
        case _ => Failure(DataHubException("Existing field entries for the object in the spreadsheet are not contiguous"))
      }
    }
  }


  def write(rawFieldEntriesObject: FieldEntriesObject, masterFieldEntriesObject: FieldEntriesObject): Try[Seq[GenericJson]] = {
    //todo better error handling
    write(IngestionStages.Raw, rawFieldEntriesObject).flatMap(x => write(IngestionStages.Master, masterFieldEntriesObject).map(Seq(x) :+ _))
  }

}

object DataDictionary {

  def apply(googleSpreadsheetId: String): Try[DataDictionary] = {
    GoogleSpreadsheet(googleSpreadsheetId).map(DataDictionary(_))
  }

}
