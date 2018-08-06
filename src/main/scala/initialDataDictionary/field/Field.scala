package initialDataDictionary.field

import dataDictionary.ObjectRow.Countries.Country
import googleSpreadsheets.{GoogleSpreadsheet, Row}
import initialDataDictionary.`object`.{Object, ObjectRowReader}
import initialDataDictionary.enumerations.TokenizationTypes.TokenizationType
import initialDataDictionary.sourceSystem.SourceSystem

import scala.util.Try

case class Field(
                  objectName: String,
                  fieldName: String,
                  index: Option[Int],
                  logicalName: String,
                  description: String,
                  dataType: String,
                  isKey: Boolean,
                  dateFormat: String,
                  length: Option[Int],
                  catalog: String,
                  conceptualEntity: String,
                  meetsTokenizationCriteria: Boolean,
                  isTDS: Option[Boolean],
                  countryTheConceptualEntity: Option[Country],
                  operationalEntity: String,
                  isMandatoryNonKey: Boolean,
                  tokenizationType: Option[TokenizationType],
                  defaultValue: String
                ) extends Row {

}

object Field {

  def apply(googleSpreadsheetId: String, sourceSystem: SourceSystem, objectNameToObject: Map[String, Object]): Try[Seq[Field]] = {
    GoogleSpreadsheet(googleSpreadsheetId).flatMap(_.get(FieldRowReader(sourceSystem, objectNameToObject)))
  }

}
