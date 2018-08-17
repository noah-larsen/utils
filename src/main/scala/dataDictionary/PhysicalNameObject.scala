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


  def objectName(physicalNameObject: String, lowercaseSourceSystems: Set[String]): Option[String] = {
    sourceSystemDataName(physicalNameObject, lowercaseSourceSystems)._2
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
