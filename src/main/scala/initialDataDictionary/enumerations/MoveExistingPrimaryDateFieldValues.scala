package initialDataDictionary.enumerations

import utils.enumerated.SelfNamed.NameFormats.ObjectNameWithSpacesBetweenWords
import utils.enumerated.{Enumerated, SelfNamed}

object MoveExistingPrimaryDateFieldValues extends Enumerated {

  override type T = MoveExistingPrimaryDateFieldValue
  sealed abstract class MoveExistingPrimaryDateFieldValue extends SelfNamed(ObjectNameWithSpacesBetweenWords())

  object No extends MoveExistingPrimaryDateFieldValue
  object NotApplicable extends MoveExistingPrimaryDateFieldValue
  object ToTheBeginning extends MoveExistingPrimaryDateFieldValue


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[MoveExistingPrimaryDateFieldValues.type], classOf[MoveExistingPrimaryDateFieldValue])

}
