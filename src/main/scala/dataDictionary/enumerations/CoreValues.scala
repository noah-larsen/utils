package dataDictionary.enumerations

import utils.enumerated.{Enumerated, SelfNamed}
import utils.enumerated.SelfNamed.NameFormats.CaseFormats.Uppercase
import utils.enumerated.SelfNamed.NameFormats.ObjectName

object CoreValues extends Enumerated {

  override type T = CoreValue
  sealed abstract class CoreValue extends SelfNamed(ObjectName(Uppercase))

  object Yes extends CoreValue
  object No extends CoreValue


  def from(isCore: Boolean): CoreValue = {
    if(isCore) Yes else No
  }


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[CoreValues.type], classOf[CoreValue])

}