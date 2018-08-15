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
                           generatedIfAlreadyDefinedWithSameLogicalFormat: Boolean,
                           generatedAtBeginning: Boolean
                         ) {

}

object GeneratedField extends RichConfig {

  def apply(generateFieldParameterNameToValue: Config): Try[GeneratedField] = Try {
    //todo better error handling
    GeneratedField(
      name = generateFieldParameterNameToValue.getString(Name.name),
      logicalName = generateFieldParameterNameToValue.getString(LogicalName.name),
      description = generateFieldParameterNameToValue.getString(Description.name),
      catalog = generateFieldParameterNameToValue.get(Catalog.name).getOrElse(new String),
      dateFormat = generateFieldParameterNameToValue.get(DateFormat.name).getOrElse(new String),
      logicalFormat = Type(generateFieldParameterNameToValue.getString(LogicalFormat.name), LogicalFormats).flatMap(_.logicalFormat).get,
      defaultValue = generateFieldParameterNameToValue.getString(DefaultValue.name),
      tokenizationType = generateFieldParameterNameToValue.get(TokenizationType.name).flatMap(TokenizationTypes.withName(_)),
      countryTheConceptualEntity = generateFieldParameterNameToValue.get(CountryTheConceptualEntity.name).flatMap(Countries.withName(_)),
      conceptualEntity = generateFieldParameterNameToValue.get(ConceptualEntity.name).getOrElse(new String),
      operationalEntity = generateFieldParameterNameToValue.get(OperationalEntity.name).getOrElse(new String),
      isTrustedDataSource = generateFieldParameterNameToValue.getBoolean(IsTrustedDataSource.name),
      generatedIfAlreadyDefinedWithSameLogicalFormat = generateFieldParameterNameToValue.getBoolean(GeneratedIfAlreadyDefinedWithSameLogicalFormat.name),
      generatedAtBeginning = generateFieldParameterNameToValue.getBoolean(PlaceAtBeginning.name)
    )
  }
  
}