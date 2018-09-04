package dataDictionary.enumerations

import utils.enumerated.{Enumerated, SelfNamed}

object SourceTypes extends Enumerated {

  override type T = SourceType

  sealed abstract case class SourceType(code: Char) extends SelfNamed {
    override def name: String = code.toString
  }


  object File extends SourceType('f')
  object Table extends SourceType('t')


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[SourceTypes.type], classOf[SourceType])

}