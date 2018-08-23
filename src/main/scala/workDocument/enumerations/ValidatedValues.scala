package workDocument.enumerations

import utils.enumerated.{Enumerated, SelfNamed}

object ValidatedValues extends Enumerated {

  override type T = ValidatedValue
  sealed abstract class ValidatedValue extends SelfNamed

  object OK extends ValidatedValue
  object KO extends ValidatedValue


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[ValidatedValues.type], classOf[ValidatedValue])

}