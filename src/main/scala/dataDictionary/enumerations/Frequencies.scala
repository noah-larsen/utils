package dataDictionary.enumerations

import utils.enumerated.{Enumerated, SelfNamed}
import utils.enumerated.SelfNamed.NameFormats.ObjectNameWithSpacesBetweenWords

object Frequencies extends Enumerated {

  override type T = Frequency
  sealed abstract class Frequency extends SelfNamed(ObjectNameWithSpacesBetweenWords())

  object Annually extends Frequency
  object Bimonthly extends Frequency
  object ByRequest extends Frequency
  object Daily extends Frequency
  object Monthly extends Frequency
  object Punctual extends Frequency
  object Quarterly extends Frequency
  object RealTime extends Frequency
  object Semiannual extends Frequency
  object Weekly extends Frequency


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[Frequencies.type], classOf[Frequency])

}