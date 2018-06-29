package us

import dataDictionary.{FieldEntriesObject, PhysicalNameObject}
import us.AlnovaTableLayouts.AlnovaSourceSystems

import scala.util.Try

object USSources {

  def fieldEntriesObject(physicalNameObject: PhysicalNameObject): Option[Try[FieldEntriesObject]] = {
    AlnovaTableLayouts.fieldEntriesObject(physicalNameObject)
  }

}
