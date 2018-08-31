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
                       addedAtBeginning: Boolean
                     ) {

}

object GeneratedField extends RichConfig {

  def apply(generatedFieldParameterNameToValue: Config): Try[GeneratedField] = Try {
    //todo better error handling
    //todo including also error handling when tokenizationType and countryTheConceptualEntity are invalid, right now it may just end up as None
    GeneratedField(
      name = generatedFieldParameterNameToValue.getString(Name.name),
      logicalName = generatedFieldParameterNameToValue.getString(LogicalName.name),
      description = generatedFieldParameterNameToValue.getString(Description.name),
      catalog = generatedFieldParameterNameToValue.getString(Catalog.name),
      dateFormat = generatedFieldParameterNameToValue.getString(DateFormat.name),
      logicalFormat = Type(generatedFieldParameterNameToValue.getString(LogicalFormat.name), LogicalFormats).flatMap(_.logicalFormat).get,
      defaultValue = generatedFieldParameterNameToValue.getString(DefaultValue.name),
      tokenizationType = TokenizationTypes.withName(generatedFieldParameterNameToValue.getString(TokenizationType.name)),
      countryTheConceptualEntity = Countries.withName(generatedFieldParameterNameToValue.getString(CountryTheConceptualEntity.name)),
      conceptualEntity = generatedFieldParameterNameToValue.getString(ConceptualEntity.name),
      operationalEntity = generatedFieldParameterNameToValue.getString(OperationalEntity.name),
      isTrustedDataSource = generatedFieldParameterNameToValue.getBoolean(IsTrustedDataSource.name),
      addedAtBeginning = generatedFieldParameterNameToValue.getBoolean(PlaceAtBeginning.name)
    )
  }
  
}