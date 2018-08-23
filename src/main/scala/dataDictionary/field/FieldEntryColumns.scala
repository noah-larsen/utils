package dataDictionary.field

import java.time.format.DateTimeFormatter

import dataDictionary.Constants
import googleSpreadsheets.{Column, Columns}

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


  private def stringOptionToString(x: Option[String]): String = {
    x.getOrElse(new String)
  }


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[FieldEntryColumns.type], classOf[FieldEntryColumn])

}