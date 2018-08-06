package googleSpreadsheets

import utils.enumerated.SelfNamed.NameFormats.CaseFormats.Uppercase
import utils.enumerated.{Enumerated, SelfNamed}

object CheckboxValues extends Enumerated {

  override type T = CheckboxValue
  sealed abstract class CheckboxValue extends SelfNamed(Uppercase)

  object True extends CheckboxValue
  object False extends CheckboxValue


  override val values = Seq(True, False)


  def toBoolean(checkboxValue: String): Boolean = {
    toBoolean(withName(checkboxValue).getOrElse(False))
  }


  def toBoolean(checkboxValue: CheckboxValue): Boolean = {
    checkboxValue.name.toBoolean
  }

}
