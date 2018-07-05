package us

import dataDictionary.{FieldEntriesObject, PhysicalNameObject}
import us.alnova.AlnovaTableLayouts
import us.phoenix.PhoenixOwnerDataDictionary

import scala.util.Try

object USSourceSystems {

  def fieldEntriesObject(physicalNameObject: PhysicalNameObject): Option[Try[FieldEntriesObject]] = {
    AlnovaTableLayouts.fieldEntriesObject(physicalNameObject).orElse(PhoenixOwnerDataDictionary.fieldEntriesObject(physicalNameObject))
  }

}
