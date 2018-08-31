package dataDictionary.field

import com.typesafe.config.Config
import dataDictionary.Type
import dataDictionary.enumerations.Countries
import dataDictionary.enumerations.Countries.Country
import dataDictionary.field.PrimaryDateFieldTemplateParameters._
import dataDictionary.types.LogicalFormats
import dataDictionary.types.LogicalFormats.LogicalFormat
import initialDataDictionary.enumerations.TokenizationTypes
import initialDataDictionary.enumerations.TokenizationTypes.TokenizationType
import utils.RichConfig

import scala.util.Try

case class PrimaryDateFieldTemplate(
                                     name: String,
                                     logicalName: String,
                                     description: String,
                                     defaultValue: String,
                                     countryTheConceptualEntity: Option[Country],
                                     conceptualEntity: String,
                                     operationalEntity: String,
                                     isTrustedDataSource: Boolean,
                                   ) {

}

object PrimaryDateFieldTemplate extends RichConfig {

  def apply(generatedFieldParameterNameToValue: Config): Try[PrimaryDateFieldTemplate] = Try {
    //todo better error handling
    PrimaryDateFieldTemplate(
      name = generatedFieldParameterNameToValue.getString(Name.name),
      logicalName = generatedFieldParameterNameToValue.getString(LogicalName.name),
      description = generatedFieldParameterNameToValue.getString(Description.name),
      defaultValue = generatedFieldParameterNameToValue.getString(DefaultValue.name),
      countryTheConceptualEntity = Countries.withName(generatedFieldParameterNameToValue.getString(CountryTheConceptualEntity.name)),
      conceptualEntity = generatedFieldParameterNameToValue.getString(ConceptualEntity.name),
      operationalEntity = generatedFieldParameterNameToValue.getString(OperationalEntity.name),
      isTrustedDataSource = generatedFieldParameterNameToValue.getBoolean(IsTrustedDataSource.name)
    )
  }

}
