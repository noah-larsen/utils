package dataDictionary.field

import java.time.format.DateTimeFormatter

import dataDictionary.Constants
import dataDictionary.enumerations.IngestionStages.IngestionStage
import dataDictionary.enumerations._
import dataDictionary.field.FieldEntryReaderWriter.FieldEntryColumns
import dataDictionary.field.FieldEntryReaderWriter.FieldEntryColumns.FieldEntryColumn
import googleSpreadsheets.{Column, Columns, RowReaderWriter, SheetRange}

import scala.util.Try

case class FieldEntryReaderWriter(ingestionStage: IngestionStage) extends RowReaderWriter[FieldEntry] {

  override def sheetRange: SheetRange = {
    SheetRange(s"DC-DD-Field-${ingestionStage match{case IngestionStages.Raw => "RAW" case IngestionStages.Master => "Master"}}", "AA", 6)
  }


  override protected def toRow(r: Int => String): FieldEntry = {
    FieldEntry(
      dropDown(Countries, r(0)),
      text(r(1)),
      dropDown(StorageTypes, r(2)),
      dropDown(StorageZones, r(3)),
      text(r(4)),
      text(r(5)),
      text(r(6)),
      text(r(7)),
      text(r(8)),
      text(r(9)),
      text(r(10)),
      dropDown(YesOrNoValues, r(11)),
      dropDown(YesOrNoValues, r(12)),
      text(r(13)),
      text(r(14)),
      text(r(15)),
      text(r(16)),
      text(r(17)),
      Some(list(r(18), Constants.listSeparator)),
      Some(Try(Some(r(19).toInt)).getOrElse(None)).filter(_.isDefined),
      dropDown(FieldGeneratedValues, r(20)),
      text(r(21)),
      Some(r(22)).filter(_ != new String).map(x => date(x, Seq(Constants.registrationDateFormat, googleSpreadsheets.googleSpreadsheetModifiedDateFormat)).get),
      dropDown(Countries, r(23)),
      text(r(24)),
      text(r(25)),
      dropDown(YesOrNoValues, r(26))
    )
  }


  override protected def columns: Seq[FieldEntryColumn] = {
    FieldEntryColumns.values
  }


  private def text(value: String): Option[String] = {
    Some(value).filter(_.trim.nonEmpty)
  }

}

object FieldEntryReaderWriter {

  object FieldEntryColumns extends Columns {

    override type RowType = FieldEntry
    override type ColumnType = FieldEntryColumn

    sealed abstract class FieldEntryColumn(val string: FieldEntry => String) extends Column[FieldEntry]


    object Country extends FieldEntryColumn(x => selfNamedOptionToString(x.country))
    object PhysicalNameObject extends FieldEntryColumn(x => stringOptionToString(x.physicalNameObject))
    object StorageType extends FieldEntryColumn(x => selfNamedOptionToString(x.storageType))
    object StorageZone extends FieldEntryColumn(x => selfNamedOptionToString(x.storageZone))
    object PhysicalNameField extends FieldEntryColumn(x => stringOptionToString(x.physicalNameField))
    object LogicalNameField extends FieldEntryColumn(x => stringOptionToString(x.logicalNameField))
    object SimpleFieldDescription extends FieldEntryColumn(x => stringOptionToString(x.simpleFieldDescription))
    object Catalog extends FieldEntryColumn(x => stringOptionToString(x.catalog))
    object DataType extends FieldEntryColumn(x => stringOptionToString(x.dataType))
    object Format extends FieldEntryColumn(x => stringOptionToString(x.format))
    object LogicalFormat extends FieldEntryColumn(x => stringOptionToString(x.logicalFormat))
    object Key extends FieldEntryColumn(x => selfNamedOptionToString(x.key))
    object Mandatory extends FieldEntryColumn(x => selfNamedOptionToString(x.mandatory))
    object DefaultValue extends FieldEntryColumn(x => stringOptionToString(x.defaultValue))
    object PhysicalNameSourceObject extends FieldEntryColumn(x => stringOptionToString(x.physicalNameSourceObject))
    object SourceField extends FieldEntryColumn(x => stringOptionToString(x.sourceField))
    object DataTypeSourceField extends FieldEntryColumn(x => stringOptionToString(x.dataTypeSourceField))
    object FormatSourceField extends FieldEntryColumn(x => stringOptionToString(x.formatSourceField))
    object Tags extends FieldEntryColumn(_.tags.map(_.mkString(Constants.listSeparator)).getOrElse(new String))
    object FieldPositionInTheObject extends FieldEntryColumn(_.fieldPositionInTheObject.flatten.map(_.toString).getOrElse(new String))
    object ExcludeInclude extends FieldEntryColumn(x => selfNamedOptionToString(x.generatedField))
    object TokenizationType extends FieldEntryColumn(x => stringOptionToString(x.tokenizationType))
    object RegistrationDate extends FieldEntryColumn(_.registrationDate.map(_.format(DateTimeFormatter.ofPattern(Constants.registrationDateFormat))).getOrElse(new String))
    object CountryTheConceptualEntity extends FieldEntryColumn(x => selfNamedOptionToString(x.countryTheConceptualEntity))
    object ConceptualEntity extends FieldEntryColumn(x => stringOptionToString(x.conceptualEntity))
    object OperationalEntity extends FieldEntryColumn(x => stringOptionToString(x.operationalEntity))
    object Tds extends FieldEntryColumn(x => selfNamedOptionToString(x.tds))


    val values = Seq(Country, PhysicalNameObject, StorageType, StorageZone, PhysicalNameField, LogicalNameField, SimpleFieldDescription, Catalog, DataType, Format, LogicalFormat, Key, Mandatory, DefaultValue, PhysicalNameSourceObject, SourceField,
      DataTypeSourceField, FormatSourceField, Tags, FieldPositionInTheObject, ExcludeInclude, TokenizationType, RegistrationDate, CountryTheConceptualEntity, ConceptualEntity, OperationalEntity, Tds
    )


    private def stringOptionToString(x: Option[String]): String = {
      x.getOrElse(new String)
    }

  }

}