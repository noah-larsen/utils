package initialDataDictionary.field

import dataDictionary.Type
import dataDictionary.enumerations.YesOrNoValues
import dataDictionary.enumerations.Countries
import dataDictionary.field.FieldRowReaderWriterColumns
import dataDictionary.field.FieldRowReaderWriterColumns._
import dataDictionary.types.LogicalFormats.{Date, Timestamp}
import googleSpreadsheets._
import initialDataDictionary.ObjectAndFields
import initialDataDictionary.`object`.Object_
import initialDataDictionary.enumerations.TokenizationTypes
import initialDataDictionary.sourceSystem.SourceSystem

case class FieldRowReaderWriter(sourceSystemForReading: SourceSystem, lcObjectNameToObject: Map[String, Object_]) extends RowReaderWriter[Field] {

  override def sheetRange: SheetRange = {
    SheetRange("Fields", "R", 4)
  }


  override protected def toRow(row: Map[Column[Field], String]): Field = {
    val objectName = row(ObjectName)
    val dataType = row(DataType)
    Field(
      objectName = objectName,
      fieldName = row(FieldName),
      index = toIntOption(row(Index)),
      logicalName = row(LogicalName),
      description = row(Description),
      dataType = dataType,
      isKey = CheckboxValues.toBoolean(row(IsKey)),
      length = toIntOption(row(Length)),
      dateFormat = withDefaultIfEmpty(row(DateFormat), dateFormatIfEmpty(dataType, objectName)),
      catalog = row(Catalog),
      conceptualEntity = row(ConceptualEntity),
      meetsTokenizationCriteria = CheckboxValues.toBoolean(row(MeetsTokenizationCriteria)),
      isTDS = withDefaultIfEmpty(row(IsTDS), YesOrNoValues.toBooleanOption, lcObjectNameToObject.get(objectName.toLowerCase).flatMap(_.isTDS)),
      isPrimaryDateField = CheckboxValues.toBoolean(row(IsPrimaryDateField)),
      countryTheConceptualEntity = withDefaultIfEmpty(row(CountryTheConceptualEntity), Countries.withName(_), sourceSystemForReading.defaultCountryTheConceptualEntity),
      operationalEntity = row(OperationalEntity),
      isMandatoryNonKey = CheckboxValues.toBoolean(row(IsMandatoryNonKey)),
      tokenizationType = TokenizationTypes.withName(row(TokenizationType)),
      defaultValue = row(DefaultValue)
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
  
}
