package initialDataDictionary.field

import dataDictionary.enumerations.Countries.Country
import dataDictionary.field.GeneratedField
import googleSpreadsheets.{GoogleSpreadsheet, Row}
import initialDataDictionary.enumerations.TokenizationTypes.TokenizationType

case class Field(
                  objectName: String,
                  fieldName: String,
                  index: Option[Int],
                  logicalName: String,
                  description: String,
                  dataType: String,
                  isKey: Boolean,
                  length: Option[Int],
                  dateFormat: String,
                  catalog: String,
                  conceptualEntity: String,
                  meetsTokenizationCriteria: Boolean,
                  isTDS: Option[Boolean],
                  isPrimaryDateField: Boolean,
                  countryTheConceptualEntity: Option[Country],
                  operationalEntity: String,
                  isMandatoryNonKey: Boolean,
                  tokenizationType: Option[TokenizationType],
                  defaultValue: String
                ) extends Row {

}
