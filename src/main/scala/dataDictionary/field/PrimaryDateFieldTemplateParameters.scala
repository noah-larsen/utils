package dataDictionary.field

import utils.enumerated.{Enumerated, SelfNamed}

object PrimaryDateFieldTemplateParameters extends Enumerated {

  override type T = PrimaryDateFieldTemplateParameter
  sealed trait PrimaryDateFieldTemplateParameter extends SelfNamed

  object Name extends PrimaryDateFieldTemplateParameter
  object LogicalName extends PrimaryDateFieldTemplateParameter
  object Description extends PrimaryDateFieldTemplateParameter
  object DefaultValue extends PrimaryDateFieldTemplateParameter
  object CountryTheConceptualEntity extends PrimaryDateFieldTemplateParameter
  object ConceptualEntity extends PrimaryDateFieldTemplateParameter
  object OperationalEntity extends PrimaryDateFieldTemplateParameter
  object IsTrustedDataSource extends PrimaryDateFieldTemplateParameter


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[PrimaryDateFieldTemplateParameters.type], classOf[PrimaryDateFieldTemplateParameter])

}