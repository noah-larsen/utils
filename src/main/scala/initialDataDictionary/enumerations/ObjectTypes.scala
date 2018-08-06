package initialDataDictionary.enumerations

import utils.enumerated.{Enumerated, SelfNamed}

object ObjectTypes extends Enumerated {

  override type T = ObjectType
  sealed abstract class ObjectType extends SelfNamed

  object File extends ObjectType
  object Table extends ObjectType


  override val values = Seq(File, Table)

}
