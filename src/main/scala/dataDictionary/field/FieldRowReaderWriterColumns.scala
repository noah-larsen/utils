package dataDictionary.field

import googleSpreadsheets.{Column, Columns}
import initialDataDictionary.field.Field

object FieldRowReaderWriterColumns extends Columns {

  override type RowType = Field
  override type ColumnType = FieldColumn
  sealed abstract class FieldColumn(val string: Field => String) extends Column[Field]

  object ObjectName extends FieldColumn(_.objectName)
  object FieldName extends FieldColumn(_.fieldName)
  object Index extends FieldColumn(x => anyValOptionToString(x.index))
  object LogicalName extends FieldColumn(_.logicalName)
  object Description extends FieldColumn(_.description)
  object DataType extends FieldColumn(_.dataType)
  object IsKey extends FieldColumn(_.isKey.toString)
  object Length extends FieldColumn(x => anyValOptionToString(x.length))
  object DateFormat extends FieldColumn(_.dateFormat)
  object Catalog extends FieldColumn(_.catalog)
  object ConceptualEntity extends FieldColumn(_.conceptualEntity)
  object MeetsTokenizationCriteria extends FieldColumn(_.meetsTokenizationCriteria.toString)
  object IsTDS extends FieldColumn(x => anyValOptionToString(x.isTDS))
  object IsPrimaryDateField extends FieldColumn(_.isPrimaryDateField.toString)
  object CountryTheConceptualEntity extends FieldColumn(x => selfNamedOptionToString(x.countryTheConceptualEntity))
  object OperationalEntity extends FieldColumn(_.operationalEntity)
  object IsMandatoryNonKey extends FieldColumn(_.isMandatoryNonKey.toString)
  object TokenizationType extends FieldColumn(x => selfNamedOptionToString(x.tokenizationType))
  object DefaultValue extends FieldColumn(_.defaultValue)


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[FieldRowReaderWriterColumns.type], classOf[FieldColumn])

}