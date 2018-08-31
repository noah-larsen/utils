package dataDictionary.field

import dataDictionary.Type
import dataDictionary.enumerations.Countries.Country
import dataDictionary.types.LogicalFormats.LogicalFormat

case class PrimaryDateField(
                             name: String,
                             logicalName: String,
                             description: String,
                             dateFormat: String,
                             logicalFormat: Type[LogicalFormat],
                             defaultValue: String,
                             countryTheConceptualEntity: Option[Country],
                             conceptualEntity: String,
                             operationalEntity: String,
                             isTrustedDataSource: Boolean,
                           ) extends {

}
