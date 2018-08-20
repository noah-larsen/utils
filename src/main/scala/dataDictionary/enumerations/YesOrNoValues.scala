package dataDictionary.enumerations

import utils.enumerated.{Enumerated, SelfNamed}
import utils.enumerated.SelfNamed.NameFormats.CaseFormats.Uppercase
import utils.enumerated.SelfNamed.NameFormats.ObjectName

object YesOrNoValues extends Enumerated {

  override type T = YesOrNo
  sealed abstract class YesOrNo extends SelfNamed(ObjectName(Uppercase))

  object Yes extends YesOrNo
  object No extends YesOrNo


  def from(boolean: Boolean): YesOrNo = {
    if(boolean) Yes else No
  }


  def toBooleanOption(yesOrNo: String): Option[Boolean] = {
    withName(yesOrNo).map(toBoolean)
  }


  def toBoolean(yesOrNo: YesOrNo): Boolean = {
    yesOrNo match {
      case Yes => true
      case No => false
    }
  }


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[YesOrNoValues.type], classOf[YesOrNo])

}