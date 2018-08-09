package initialDataDictionary.field

import dataDictionary.enumerations.Countries.Country
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
