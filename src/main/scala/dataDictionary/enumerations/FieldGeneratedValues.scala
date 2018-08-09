package dataDictionary.enumerations

import utils.enumerated.{Enumerated, SelfNamed}
import utils.enumerated.SelfNamed.NameFormats.CaseFormats.Uppercase
import utils.enumerated.SelfNamed.NameFormats.ObjectName

object FieldGeneratedValues extends Enumerated {

  override type T = FieldGeneratedValue
  sealed abstract class FieldGeneratedValue extends SelfNamed(ObjectName(Uppercase))

  object Yes extends FieldGeneratedValue


  override val values = Seq(Yes)

}