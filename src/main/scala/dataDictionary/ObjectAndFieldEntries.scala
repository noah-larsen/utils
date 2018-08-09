package dataDictionary

import dataDictionary.enumerations.{DataTypes, YesOrNoValues}
import dataDictionary.enumerations.IngestionStages.{Master, Raw}
import dataDictionary.types.SuperTypes
import initialDataDictionary.ObjectAndFields

case class ObjectAndFieldEntries(
                                  rawObjectEntry: ObjectEntry,
                                  masterObjectEntry: ObjectEntry,
                                  rawFieldEntriesObject: FieldEntriesObject,
                                  masterFieldEntriesObject: FieldEntriesObject
                                ) {

}

object ObjectAndFieldEntries {

  def apply(objectAndFields: ObjectAndFields): ObjectAndFieldEntries = {
    val rawFieldEntriesObject = FieldEntriesObject.rawFEOFromTextExtraction(objectAndFields)
    ObjectAndFieldEntries(ObjectEntry(objectAndFields.obj, objectAndFields.sourceSystem, Raw), ObjectEntry(objectAndFields.obj, objectAndFields.sourceSystem, Master), rawFieldEntriesObject, rawFieldEntriesObject.toMasterFromTextExtraction(objectAndFields.fields
      .map(x => (x.fieldName, x.dateFormat)).toMap))
  }

}
