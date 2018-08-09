package dataDictionary

import java.time.LocalDate

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

  def updateFieldEntriesIfFromTextExtraction(rawFieldEntriesObject: FieldEntriesObject): ObjectAndFieldEntries = {
    val sourceFieldToDateFormat = masterFieldEntriesObject.fieldEntries.collect{case x if x.physicalNameField.isDefined && x.isDateOrTime.contains(true) && x.format.isDefined => rawFieldEntriesObject.fieldEntries.find(_.physicalNameField.exists(_
      .equalsIgnoreCase(x.physicalNameField.get))).flatMap(_.sourceField) -> x.format.get}.collect{case (Some(x), y) => (x, y)}.toMap
    ObjectAndFieldEntries(rawObjectEntry, masterObjectEntry, rawFieldEntriesObject, rawFieldEntriesObject.toMasterIfFromTextExtraction(sourceFieldToDateFormat))
  }


  def withRegistrationDates: ObjectAndFieldEntries = {
    ObjectAndFieldEntries(rawObjectEntry.withRegistrationDate, masterObjectEntry.withRegistrationDate, rawFieldEntriesObject.withRegistrationDates, masterFieldEntriesObject.withRegistrationDates)
  }

}

object ObjectAndFieldEntries {

  def apply(objectAndFields: ObjectAndFields): ObjectAndFieldEntries = {
    val rawFieldEntriesObject = FieldEntriesObject.rawFEOFromTextExtraction(objectAndFields)
    ObjectAndFieldEntries(ObjectEntry(objectAndFields.obj, objectAndFields.sourceSystem, Raw), ObjectEntry(objectAndFields.obj, objectAndFields.sourceSystem, Master), rawFieldEntriesObject, rawFieldEntriesObject.toMasterIfFromTextExtraction(objectAndFields.fields
      .map(x => (x.fieldName, x.dateFormat)).toMap))
  }

}
