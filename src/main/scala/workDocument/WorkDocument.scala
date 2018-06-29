package workDocument

import com.google.api.client.json.GenericJson
import general.DataHubException
import googleSpreadsheets.GoogleSpreadsheet

import scala.util.{Failure, Try}

case class WorkDocument(private val spreadsheet: GoogleSpreadsheet) {

  def entries: Try[Seq[WorkDocumentEntry]] = {
    spreadsheet.get[WorkDocumentEntry](WorkDocumentEntry)
  }


  def entriesObjects: Try[Seq[WorkDocumentEntriesObject]] = {
    entries.map(_.groupBy(_.table).values.map(WorkDocumentEntriesObject(_)).toSeq)
  }


  def approvedEntriesObjects: Try[Seq[WorkDocumentEntriesObject]] = {
    entriesObjects.map(_.filter(_.validatedByLocalAndGlobalArchitecture))
  }


  def writeOnce(workDocumentEntriesObject: WorkDocumentEntriesObject): Try[GenericJson] = {
    entries.flatMap(_.find(_.table == workDocumentEntriesObject.table)
      .map(_ => Failure(DataHubException("Entries for the table already exist in work document.")))
      .getOrElse(spreadsheet.append(workDocumentEntriesObject.entries, WorkDocumentEntry))
    )
  }

}

object WorkDocument {

  def apply(googleSpreadsheetId: String): Try[WorkDocument] = {
    GoogleSpreadsheet(googleSpreadsheetId).map(WorkDocument(_))
  }

}
