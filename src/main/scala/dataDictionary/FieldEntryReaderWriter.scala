package dataDictionary

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import dataDictionary.FieldEntry.IngestionStages.IngestionStage
import dataDictionary.FieldEntryReaderWriter.FieldEntryColumns
import dataDictionary.FieldEntry._
import dataDictionary.ObjectRow.{Countries, StorageTypes, StorageZones}
import googleSpreadsheets.DataReaderWriter.{AbstractColumn, AbstractColumns}
import googleSpreadsheets.{DataReaderWriter, SheetRange}

import scala.util.Try

case class FieldEntryReaderWriter(ingestionStage: IngestionStage) extends DataReaderWriter[FieldEntry] {

  override def sheetRange: SheetRange = {
    SheetRange(s"DC-DD-Field-${ingestionStage match{case IngestionStages.Raw => "RAW" case IngestionStages.Master => "Master"}}", "AA", 5)
  }


  override protected def toRow(row: Int => String): FieldEntry = {
    FieldEntry(
      dropDown(Countries, row(0)),
      text(row(1)),
      dropDown(StorageTypes, row(2)),
      dropDown(StorageZones, row(3)),
      text(row(4)),
      text(row(5)),
      text(row(6)),
      text(row(7)),
      text(row(8)),
      text(row(9)),
      text(row(10)),
      dropDown(FieldRowBooleans, row(11)),
      dropDown(FieldRowBooleans, row(12)),
      text(row(13)),
      text(row(14)),
      text(row(15)),
      text(row(16)),
      text(row(17)),
      Some(row(18).split(FieldEntryReaderWriter.tagsSeparator).toSeq).filter(_.exists(x => x.trim != new String)),
      Some(Try(Some(row(19).toInt)).getOrElse(None)).filter(_.isDefined),
      dropDown(FieldGeneratedValues, row(20)),
      text(row(21)),
      Some(row(22)).filter(_ != new String).map(LocalDate.parse(_, DateTimeFormatter.ofPattern(FieldEntryReaderWriter.registrationDateFormat))),
      dropDown(Countries, row(23)),
      text(row(24)),
      text(row(25)),
      dropDown(FieldRowBooleans, row(26))
    )
  }


  override protected def columns: AbstractColumns[FieldEntry] = FieldEntryColumns


  private def text(value: String): Option[String] = {
    Some(value).filter(_.trim.nonEmpty)
  }

}

object FieldEntryReaderWriter {

    object FieldEntryColumns extends AbstractColumns[FieldEntry] {

      override type Column = FieldEntryColumn

      sealed class FieldEntryColumn(val string: FieldEntry => String) extends AbstractColumn[FieldEntry]


      case object Country extends FieldEntryColumn(x => enumeratedTypeOptionToString(x.country))
      case object PhysicalNameObject extends FieldEntryColumn(x => stringOptionToString(x.physicalNameObject))
      case object StorageType extends FieldEntryColumn(x => enumeratedTypeOptionToString(x.storageType))
      case object StorageZone extends FieldEntryColumn(x => enumeratedTypeOptionToString(x.storageZone))
      case object PhysicalNameField extends FieldEntryColumn(x => stringOptionToString(x.physicalNameField))
      case object LogicalNameField extends FieldEntryColumn(x => stringOptionToString(x.logicalNameField))
      case object SimpleFieldDescription extends FieldEntryColumn(x => stringOptionToString(x.simpleFieldDescription))
      case object Catalog extends FieldEntryColumn(x => stringOptionToString(x.catalog))
      case object DataType extends FieldEntryColumn(x => stringOptionToString(x.dataType))
      case object Format extends FieldEntryColumn(x => stringOptionToString(x.format))
      case object LogicalFormat extends FieldEntryColumn(x => stringOptionToString(x.logicalFormat))
      case object Key extends FieldEntryColumn(x => enumeratedTypeOptionToString(x.key))
      case object Mandatory extends FieldEntryColumn(x => enumeratedTypeOptionToString(x.mandatory))
      case object DefaultValue extends FieldEntryColumn(x => stringOptionToString(x.defaultValue))
      case object PhysicalNameSourceObject extends FieldEntryColumn(x => stringOptionToString(x.physicalNameSourceObject))
      case object SourceField extends FieldEntryColumn(x => stringOptionToString(x.sourceField))
      case object DataTypeSourceField extends FieldEntryColumn(x => stringOptionToString(x.dataTypeSourceField))
      case object FormatSourceField extends FieldEntryColumn(x => stringOptionToString(x.formatSourceField))
      case object Tags extends FieldEntryColumn(_.tags.map(_.mkString(tagsSeparator)).getOrElse(new String))
      case object FieldPositionInTheObject extends FieldEntryColumn(_.fieldPositionInTheObject.flatten.map(_.toString).getOrElse(new String))
      case object ExcludeInclude extends FieldEntryColumn(x => enumeratedTypeOptionToString(x.excludeInclude))
      case object TokenizationType extends FieldEntryColumn(x => stringOptionToString(x.tokenizationType))
      case object RegistrationDate extends FieldEntryColumn(_.registrationDate.map(_.format(DateTimeFormatter.ofPattern(registrationDateFormat))).getOrElse(new String))
      case object CountryTheConceptualEntity extends FieldEntryColumn(x => enumeratedTypeOptionToString(x.countryTheConceptualEntity))
      case object ConceptualEntity extends FieldEntryColumn(x => stringOptionToString(x.conceptualEntity))
      case object OperationalEntity extends FieldEntryColumn(x => stringOptionToString(x.operationalEntity))
      case object Tds extends FieldEntryColumn(x => enumeratedTypeOptionToString(x.tds))


      val order = Seq(Country, PhysicalNameObject, StorageType, StorageZone, PhysicalNameField, LogicalNameField, SimpleFieldDescription, Catalog, DataType, Format, LogicalFormat, Key, Mandatory, DefaultValue, PhysicalNameSourceObject, SourceField,
        DataTypeSourceField, FormatSourceField, Tags, FieldPositionInTheObject, ExcludeInclude, TokenizationType, RegistrationDate, CountryTheConceptualEntity, ConceptualEntity, OperationalEntity, Tds
      )


      private def stringOptionToString(x: Option[String]): String = {
        x.getOrElse(new String)
      }

    }


    private val tagsSeparator = ";"
    private val registrationDateFormat = "dd-MM-yyyy"

}