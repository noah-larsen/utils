package initialDataDictionary.field

import dataDictionary.enumerations.YesOrNoValues
import dataDictionary.enumerations.Countries
import googleSpreadsheets.{CheckboxValues, RowReader, SheetRange}
import initialDataDictionary.`object`.Object_
import initialDataDictionary.enumerations.TokenizationTypes
import initialDataDictionary.sourceSystem.SourceSystem

case class FieldRowReader(sourceSystem: SourceSystem, objectNameToObject: Map[String, Object_]) extends RowReader[Field] {

  override def sheetRange: SheetRange = {
    SheetRange("Fields", "R", 4)
  }

  override protected def toRow(r: Int => String): Field = {
    Field(
      objectName = r(0),
      fieldName = r(1),
      index = toIntOption(r(2)),
      logicalName = r(3),
      description = r(4),
      dataType = r(5),
      isKey = CheckboxValues.toBoolean(r(6)),
      dateFormat = r(7),
      length = toIntOption(r(8)),
      catalog = r(9),
      conceptualEntity = r(10),
      meetsTokenizationCriteria = CheckboxValues.toBoolean(r(11)),
      isTDS = withDefaultIfEmpty(r(12), YesOrNoValues.toBooleanOption, objectNameToObject.get(r(0)).flatMap(_.isTDS)),
      countryTheConceptualEntity = withDefaultIfEmpty(r(13), Countries.withName(_), sourceSystem.defaultCountryTheConceptualEntity),
      operationalEntity = r(14),
      isMandatoryNonKey = CheckboxValues.toBoolean(r(15)),
      tokenizationType = TokenizationTypes.withName(r(16)),
      defaultValue = r(17)
    )
  }


  private def toIntOption(value: String): Option[Int] = {
    Some(value).filter(_.nonEmpty).map(_.toInt)
  }

}
