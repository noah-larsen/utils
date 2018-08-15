package dataDictionary.field

import dataDictionary.enumerations.Countries.Country
import dataDictionary.field.GeneratedFieldParameters.GeneratedFieldParameter
import utils.enumerated.{Enumerated, SelfNamed}

object GeneratedFieldParameters extends Enumerated {

  override type T = GeneratedFieldParameter
  sealed trait GeneratedFieldParameter extends SelfNamed

  object Name extends GeneratedFieldParameter
  object LogicalName extends GeneratedFieldParameter
  object Description extends GeneratedFieldParameter
  object Catalog extends GeneratedFieldParameter
  object DateFormat extends GeneratedFieldParameter
  object LogicalFormat extends GeneratedFieldParameter
  object DefaultValue extends GeneratedFieldParameter
  object TokenizationType extends GeneratedFieldParameter
  object CountryTheConceptualEntity extends GeneratedFieldParameter
  object ConceptualEntity extends GeneratedFieldParameter
  object OperationalEntity extends GeneratedFieldParameter
  object IsTrustedDataSource extends GeneratedFieldParameter
  object GeneratedIfAlreadyDefinedWithSameLogicalFormat extends GeneratedFieldParameter
  object PlaceAtBeginning extends GeneratedFieldParameter


  override val values = Seq(Name, LogicalName, Description, Catalog, DateFormat, LogicalFormat, DefaultValue, TokenizationType, CountryTheConceptualEntity, ConceptualEntity, OperationalEntity, IsTrustedDataSource, GeneratedIfAlreadyDefinedWithSameLogicalFormat,
    PlaceAtBeginning)

}