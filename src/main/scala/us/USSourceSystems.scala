package us

import dataDictionary.{FieldEntriesObject, PhysicalNameObject}
import us.alnova.AlnovaTableLayouts
import us.phoenix.PhoenixDataDictionary

import scala.util.Try

object USSourceSystems {

  def fieldEntriesObject(physicalNameObject: PhysicalNameObject): Option[Try[FieldEntriesObject]] = {
    AlnovaTableLayouts.fieldEntriesObject(physicalNameObject).orElse(PhoenixDataDictionary.fieldEntriesObject(physicalNameObject))
  }

}
