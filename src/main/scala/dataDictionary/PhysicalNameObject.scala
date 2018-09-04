package dataDictionary

import dataDictionary.enumerations.SourceTypes
import dataDictionary.enumerations.SourceTypes.SourceType

case class PhysicalNameObject(
                               sourceType: SourceType,
                               systemCodeUUAA: String,
                               sourceSystem: String,
                               objectName: String
                             ) {

  def asLcString: String = {
    asString.toLowerCase
  }


  def asString: String = {
    Seq(sourceType.code, systemCodeUUAA, sourceSystem, objectName).map(_.toString.toLowerCase).mkString(PhysicalNameObject.physicalNameObjectSeparator)
  }


  def lcObjectName: String = {
    objectName.toLowerCase
  }


  def lcSourceSystem: String = {
    sourceSystem.toLowerCase
  }

}

object PhysicalNameObject {

  //todo better error handling, also many strings can still break sourceSystem and nameData

  def apply(physicalNameObject: String, lcSourceSystems: Set[String]): Option[PhysicalNameObject] = {
    (sourceType(physicalNameObject), applicationId(physicalNameObject), sourceSystem(physicalNameObject, lcSourceSystems), objectName(physicalNameObject, lcSourceSystems)) match {
      case (Some(w), Some(x), Some(y), Some(z)) => Some(PhysicalNameObject(w, x, y, z))
      case _ => None
    }
  }


  def sourceType(physicalNameObject: String): Option[SourceType] = {
    SourceTypes.withName(physicalNameObject.split(physicalNameObjectSeparator).head)
  }


  def applicationId(physicalNameObject: String): Option[String] = {
    val applicationIdIndex = 1
    Some(physicalNameObject.split(physicalNameObjectSeparator)).filter(_.length > applicationIdIndex).map(_(applicationIdIndex))
  }


  def sourceSystem(physicalNameObject: String, lowercaseSourceSystems: Set[String]): Option[String] = {
    sourceSystemAndObjectName(physicalNameObject, lowercaseSourceSystems)._1
  }


  def objectName(physicalNameObject: String, lowercaseSourceSystems: Set[String]): Option[String] = {
    sourceSystemAndObjectName(physicalNameObject, lowercaseSourceSystems)._2
  }


  private val physicalNameObjectSeparator = "_"


  private def sourceSystemAndObjectName(physicalNameObject: String, lowercaseSourceSystems: Set[String]): (Option[String], Option[String]) = {
    val substrings = physicalNameObject.split(physicalNameObjectSeparator)
    val sourceSystemAndObjectNameSubstrings = substrings.tail.tail
    val sourceSystem = sourceSystemAndObjectNameSubstrings.inits.toSeq.reverse.init.tail.map(_.mkString(physicalNameObjectSeparator)).find(lowercaseSourceSystems.contains)
    val sourceSystemAndObjectName = sourceSystemAndObjectNameSubstrings.mkString(physicalNameObjectSeparator)
    val objectName = sourceSystem.filter(_.length != sourceSystemAndObjectName.length).map(x => sourceSystemAndObjectName.substring(x.length + physicalNameObjectSeparator.length))
    (sourceSystem, objectName)
  }

}
