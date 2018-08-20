package initialDataDictionary.field

import dataDictionary.Type
import dataDictionary.enumerations.YesOrNoValues
import dataDictionary.enumerations.Countries
import dataDictionary.types.LogicalFormats.{Date, Timestamp}
import googleSpreadsheets._
import initialDataDictionary.ObjectAndFields
import initialDataDictionary.`object`.Object_
import initialDataDictionary.enumerations.TokenizationTypes
import initialDataDictionary.field.FieldRowReaderWriter.FieldRowReaderWriterColumns
import initialDataDictionary.sourceSystem.SourceSystem

case class FieldRowReaderWriter(sourceSystemForReading: SourceSystem, lcObjectNameToObject: Map[String, Object_]) extends RowReaderWriter[Field] {

  override def sheetRange: SheetRange = {
    SheetRange("Fields", "R", 4)
  }


  override protected def toRow(r: Int => String): Field = {
    val objectName = r(0)
    val dataType = r(5)
    Field(
      objectName = objectName,
      fieldName = r(1),
      index = toIntOption(r(2)),
      logicalName = r(3),
      description = r(4),
      dataType = dataType,
      isKey = CheckboxValues.toBoolean(r(6)),
      dateFormat = withDefaultIfEmpty(r(7), dateFormatIfEmpty(dataType, objectName)),
      length = toIntOption(r(8)),
      catalog = r(9),
      conceptualEntity = r(10),
      meetsTokenizationCriteria = CheckboxValues.toBoolean(r(11)),
      isTDS = withDefaultIfEmpty(r(12), YesOrNoValues.toBooleanOption, lcObjectNameToObject.get(r(0).toLowerCase).flatMap(_.isTDS)),
      isPrimaryDateField = CheckboxValues.toBoolean(r(13)),
      countryTheConceptualEntity = withDefaultIfEmpty(r(14), Countries.withName(_), sourceSystemForReading.defaultCountryTheConceptualEntity),
      operationalEntity = r(15),
      isMandatoryNonKey = CheckboxValues.toBoolean(r(16)),
      tokenizationType = TokenizationTypes.withName(r(17)),
      defaultValue = r(18)
    )
  }


  private def toIntOption(value: String): Option[Int] = {
    Some(value).filter(_.nonEmpty).map(_.toInt)
  }


  private def dateFormatIfEmpty(dataType: String, objectName: String): String = {
    lcObjectNameToObject.get(objectName.toLowerCase).flatMap(_.dataSuperType).flatMap(Type.logicalFormat(dataType, _).map(_.typeType)).collect{
      case Date => sourceSystemForReading.defaultDateFormat
      case Timestamp => sourceSystemForReading.defaultTimestampFormat
    }.getOrElse(new String)
  }


  override protected def columns: Seq[Column[Field]] = {
    FieldRowReaderWriterColumns.values
  }

}

object FieldRowReaderWriter {

  def apply(objectsAndFields: Seq[ObjectAndFields]): FieldRowReaderWriter = {
    FieldRowReaderWriter(objectsAndFields.headOption.map(_.sourceSystem).getOrElse(SourceSystem.empty), objectsAndFields.map(x => (x.obj.objectName.toLowerCase, x.obj)).toMap)
  }


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
    object DateFormat extends FieldColumn(_.dateFormat)
    object Length extends FieldColumn(x => anyValOptionToString(x.length))
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
  
}
