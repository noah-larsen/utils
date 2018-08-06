package initialDataDictionary.field

import dataDictionary.ObjectRow.Countries.Country

case class Field(
                  objectName: String,
                  fieldName: String,
                  index: Int,
                  logicalName: String,
                  description: String,
                  dataType: String,
                  isKey: Boolean,
                  dateFormat: String,
                  length: String,
                  catalog: String,
                  conceptualEntity: String,
                  meetsTokenizationCriteria: Boolean,
                  isTDS: Boolean,
                  countryTheConceptualEntity: Country,
                  operationalEntity: String,
                  isMandatoryNonKey: Boolean,
                  tokenizationType: Option[_],
                  defaultValue: String
                ) {

}
