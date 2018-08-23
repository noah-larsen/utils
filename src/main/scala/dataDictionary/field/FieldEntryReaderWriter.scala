package dataDictionary.field

import java.time.format.DateTimeFormatter

import dataDictionary.Constants
import dataDictionary.enumerations.IngestionStages.IngestionStage
import dataDictionary.enumerations._
import dataDictionary.field.FieldEntryColumns._
import googleSpreadsheets.{Column, Columns, RowReaderWriter, SheetRange}

import scala.util.Try

case class FieldEntryReaderWriter(ingestionStage: IngestionStage) extends RowReaderWriter[FieldEntry] {

  override def sheetRange: SheetRange = {
    SheetRange(s"DC-DD-Field-${ingestionStage match{case IngestionStages.Raw => "RAW" case IngestionStages.Master => "Master"}}", "AA", 6)
  }


  override protected def toRow(row: Map[Column[FieldEntry], String]): FieldEntry = {
    FieldEntry(
      dropDown(Countries, row(Country)),
      text(row(PhysicalNameObject)),
      dropDown(StorageTypes, row(StorageType)),
      dropDown(StorageZones, row(StorageZone)),
      text(row(PhysicalNameField)),
      text(row(LogicalNameField)),
      text(row(SimpleFieldDescription)),
      text(row(Catalog)),
      text(row(DataType)),
      text(row(Format)),
      text(row(LogicalFormat)),
      dropDown(YesOrNoValues, row(Key)),
      dropDown(YesOrNoValues, row(Mandatory)),
      text(row(DefaultValue)),
      text(row(PhysicalNameSourceObject)),
      text(row(SourceField)),
      text(row(DataTypeSourceField)),
      text(row(FormatSourceField)),
      Some(list(row(Tags), Constants.listSeparator)),
      Some(Try(Some(row(FieldPositionInTheObject).toInt)).getOrElse(None)).filter(_.isDefined),
      dropDown(FieldGeneratedValues, row(ExcludeInclude)),
      text(row(TokenizationType)),
      Some(row(RegistrationDate)).filter(_ != new String).map(x => date(x, Seq(Constants.registrationDateFormat, googleSpreadsheets.googleSpreadsheetModifiedDateFormat)).get),
      dropDown(Countries, row(CountryTheConceptualEntity)),
      text(row(ConceptualEntity)),
      text(row(OperationalEntity)),
      dropDown(YesOrNoValues, row(Tds))
    )
  }


  override protected def columns: Seq[FieldEntryColumn] = {
    FieldEntryColumns.values
  }


  private def text(value: String): Option[String] = {
    Some(value).filter(_.trim.nonEmpty)
  }

}
