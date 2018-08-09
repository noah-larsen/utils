package dataDictionary.enumerations

import utils.enumerated.{Enumerated, SelfNamed}
import utils.enumerated.SelfNamed.NameFormats.ObjectNameWithSpacesBetweenWords

object Countries extends Enumerated {

  override type T = Country
  sealed abstract class Country extends SelfNamed(ObjectNameWithSpacesBetweenWords())

  object Argentina extends Country
  object Colombia extends Country
  object Chile extends Country
  object Holding extends Country
  object Mexico extends Country
  object Paraguay extends Country
  object Peru extends Country
  object Spain extends Country
  object Turkey extends Country
  object UnitedStates extends Country
  object Uruguay extends Country
  object Venezuela extends Country


  override val values = Seq(Argentina, Colombia, Chile, Holding, Mexico, Paraguay, Peru, Spain, Turkey, UnitedStates, Uruguay, Venezuela)

}