package dataDictionary.enumerations

import utils.enumerated.{Enumerated, SelfNamed}

object LoadingTypes extends Enumerated {

  override type T = LoadingType
  sealed abstract class LoadingType extends SelfNamed

  object Complete extends LoadingType
  object Incremental extends LoadingType


  override val values = Seq(Complete, Incremental)

}