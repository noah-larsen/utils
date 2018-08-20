package initialDataDictionary.enumerations

import dataDictionary.enumerations.SourceTypes
import dataDictionary.enumerations.SourceTypes.SourceType
import utils.enumerated.{Enumerated, SelfNamed}

object ObjectTypes extends Enumerated {

  override type T = ObjectType
  sealed abstract class ObjectType extends SelfNamed

  object File extends ObjectType
  object Table extends ObjectType


  def toSourceType(objectType: ObjectType): SourceType = {
    objectType match {
      case File => SourceTypes.File
      case Table => SourceTypes.Table
    }
  }


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[ObjectTypes.type], classOf[ObjectType])

}
