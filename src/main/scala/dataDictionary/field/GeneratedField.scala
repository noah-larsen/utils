package dataDictionary.field

import com.typesafe.config.{Config, ConfigObject}
import dataDictionary.Type
import dataDictionary.enumerations.Countries
import dataDictionary.enumerations.Countries.Country
import dataDictionary.field.GeneratedFieldParameters._
import dataDictionary.types.LogicalFormats
import dataDictionary.types.LogicalFormats.LogicalFormat
import initialDataDictionary.enumerations.TokenizationTypes
import initialDataDictionary.enumerations.TokenizationTypes.TokenizationType
import utils.RichConfig

import scala.util.Try

case class GeneratedField(
                           name: String,
                           logicalName: String,
                           description: String,
                           catalog: String,
                           dateFormat: String,
                           logicalFormat: Type[LogicalFormat],
                           defaultValue: String,
                           tokenizationType: Option[TokenizationType],
                           countryTheConceptualEntity: Option[Country],
                           conceptualEntity: String,
                           operationalEntity: String,
                           isTrustedDataSource: Boolean,
                           generatedAtBeginning: Boolean
                         ) {

}

object GeneratedField extends RichConfig {

  def apply(generateFieldParameterNameToValue: Config): Try[GeneratedField] = Try {
    //todo better error handling
    //todo including also error handling when tokenizationType and countryTheConceptualEntity are invalid, right now it may just end up as None
    GeneratedField(
      name = generateFieldParameterNameToValue.getString(Name.name),
      logicalName = generateFieldParameterNameToValue.getString(LogicalName.name),
      description = generateFieldParameterNameToValue.getString(Description.name),
      catalog = generateFieldParameterNameToValue.getString(Catalog.name),
      dateFormat = generateFieldParameterNameToValue.getString(DateFormat.name),
      logicalFormat = Type(generateFieldParameterNameToValue.getString(LogicalFormat.name), LogicalFormats).flatMap(_.logicalFormat).get,
      defaultValue = generateFieldParameterNameToValue.getString(DefaultValue.name),
      tokenizationType = TokenizationTypes.withName(generateFieldParameterNameToValue.getString(TokenizationType.name)),
      countryTheConceptualEntity = Countries.withName(generateFieldParameterNameToValue.getString(CountryTheConceptualEntity.name)),
      conceptualEntity = generateFieldParameterNameToValue.getString(ConceptualEntity.name),
      operationalEntity = generateFieldParameterNameToValue.getString(OperationalEntity.name),
      isTrustedDataSource = generateFieldParameterNameToValue.getBoolean(IsTrustedDataSource.name),
      generatedAtBeginning = generateFieldParameterNameToValue.getBoolean(PlaceAtBeginning.name)
    )
  }
  
}