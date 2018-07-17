package dataDictionary

import dataDictionary.PhysicalNameObject.SourceTypes.SourceType
import utils.enumerated.Enumerated
import utils.enumerated.EnumeratedType

case class PhysicalNameObject(
                               sourceType: SourceType,
                               applicationId: String,
                               sourceSystem: String,
                               dataName: String
                             ) {

  def string: String = {
    Seq(sourceType.code, applicationId, sourceSystem, dataName).map(_.toString.toLowerCase).mkString(PhysicalNameObject.physicalNameObjectSeparator)
  }

}

object PhysicalNameObject {

  //todo better error handling, also many strings can still break sourceSystem and nameData

  def sourceType(physicalNameObject: String): Option[SourceType] = {
    SourceTypes.withName(physicalNameObject.split(physicalNameObjectSeparator).head)
  }


  def applicationId(physicalNameObject: String): Option[String] = {
    val applicationIdIndex = 1
    Some(physicalNameObject.split(physicalNameObjectSeparator)).filter(_.length > applicationIdIndex).map(_(applicationIdIndex))
  }


  def sourceSystem(physicalNameObject: String, lowercaseSourceSystems: Set[String]): Option[String] = {
    sourceSystemDataName(physicalNameObject, lowercaseSourceSystems)._1
  }


  def dataName(physicalNameObject: String, lowercaseSourceSystems: Set[String]): Option[String] = {
    sourceSystemDataName(physicalNameObject, lowercaseSourceSystems)._2
  }


  object SourceTypes extends Enumerated {

    override type T = SourceType

    sealed case class SourceType(code: Char) extends EnumeratedType {
      override def name: String = code.toString
    }


    object File extends SourceType('f')
    object Table extends SourceType('t')


    override val values = Seq(File, Table)

  }


  private val physicalNameObjectSeparator = "_"


  private def sourceSystemDataName(physicalNameObject: String, lowercaseSourceSystems: Set[String]): (Option[String], Option[String]) = {
    val substrings = physicalNameObject.split(physicalNameObjectSeparator)
    val sourceSystemDataNameSubstrings = substrings.tail.tail
    val sourceSystem = sourceSystemDataNameSubstrings.inits.toSeq.reverse.init.tail.map(_.mkString(physicalNameObjectSeparator)).find(lowercaseSourceSystems.contains)
    val sourceSystemDataName = sourceSystemDataNameSubstrings.mkString(physicalNameObjectSeparator)
    val dataName = sourceSystem.filter(_.length != sourceSystemDataName.length).map(x => sourceSystemDataNameSubstrings.mkString(physicalNameObjectSeparator).substring(x.length))
    (sourceSystem, dataName)
  }

}
