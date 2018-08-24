package dataDictionary.`object`

import dataDictionary.enumerations.FieldGeneratedValues
import dataDictionary.enumerations.IngestionStages.{Master, Raw}
import dataDictionary.field.{FieldEntriesObject, GeneratedField}
import initialDataDictionary.ObjectAndFields

case class ObjectAndFieldEntries(
                                  rawObjectEntry: ObjectEntry,
                                  masterObjectEntry: ObjectEntry,
                                  rawFieldEntriesObject: FieldEntriesObject,
                                  masterFieldEntriesObject: FieldEntriesObject
                                ) {

  def updateFieldEntriesIfFromTextExtraction(rawFieldEntriesObject: FieldEntriesObject): ObjectAndFieldEntries = {
    val lcSourceFieldToDateFormat = masterFieldEntriesObject.fieldEntries.collect{case x if x.physicalNameField.isDefined && x.isDateOrTimestamp.contains(true) && x.format.isDefined => rawFieldEntriesObject.fieldEntries.find(_.physicalNameField.exists(_
      .equalsIgnoreCase(x.physicalNameField.get))).flatMap(_.sourceField) -> x.format.get}.collect{case (Some(x), y) => (x.toLowerCase, y)}.toMap
    val lcGeneratedFieldNameToDateFormat = masterFieldEntriesObject.fieldEntries.collect{case x if x.physicalNameField.isDefined && x.isDateOrTimestamp.contains(true) && x.format.isDefined => rawFieldEntriesObject.fieldEntries.find(_.physicalNameField.exists(_
      .equalsIgnoreCase(x.physicalNameField.get))).collect{case y if y.generatedField.contains(FieldGeneratedValues.Yes) && y.physicalNameField.isDefined => y.physicalNameField.get} -> x.format.get}.collect{case (Some(x), y) => (x.toLowerCase, y)}.toMap
    val lcMandatoryNonKeySourceFields = masterFieldEntriesObject.fieldEntries.collect{case x if x.isMandatory.contains(true) && !x.isKey.contains(true) && x.physicalNameField.isDefined => rawFieldEntriesObject.fieldEntries.find(_.physicalNameField.exists(_
      .equalsIgnoreCase(x.physicalNameField.get))).flatMap(_.sourceField.filter(_.nonEmpty).map(_.toLowerCase))}.collect{case Some(x) => x}.toSet
    ObjectAndFieldEntries(rawObjectEntry, masterObjectEntry, rawFieldEntriesObject, rawFieldEntriesObject.toMasterIfFromTextExtraction(lcSourceFieldToDateFormat, lcGeneratedFieldNameToDateFormat, lcMandatoryNonKeySourceFields))
  }


  def withRegistrationDates: ObjectAndFieldEntries = {
    ObjectAndFieldEntries(rawObjectEntry.withRegistrationDate, masterObjectEntry.withRegistrationDate, rawFieldEntriesObject.withRegistrationDates, masterFieldEntriesObject.withRegistrationDates)
  }

}

object ObjectAndFieldEntries {

  def fromTextExtraction(objectAndFields: ObjectAndFields, generatedFields: Seq[GeneratedField], primaryDateField: Option[GeneratedField]): ObjectAndFieldEntries = {
    val rawFieldEntriesObject = FieldEntriesObject.rawFEOFromTextExtraction(objectAndFields, generatedFields, primaryDateField)
    ObjectAndFieldEntries(ObjectEntry(objectAndFields.obj, objectAndFields.sourceSystem, Raw), ObjectEntry(objectAndFields.obj, objectAndFields.sourceSystem, Master), rawFieldEntriesObject, rawFieldEntriesObject.toMasterIfFromTextExtraction(objectAndFields
      .fields.map(x => (x.fieldName.toLowerCase, x.dateFormat)).toMap, generatedFields.map(x => (x.name.toLowerCase, x.dateFormat)).toMap, objectAndFields.fields.collect{case x if x.isMandatoryNonKey => x.fieldName.toLowerCase}.toSet))
  }

}
