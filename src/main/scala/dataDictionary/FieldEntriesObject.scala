package dataDictionary

import java.time.LocalDate

import dataDictionary.FieldEntry.FieldRowBooleans.FieldRowBoolean
import dataDictionary.FieldEntry.{DataTypes, FieldGeneratedValues, FieldRowBooleans}
import dataDictionary.FieldEntryReaderWriter.FieldEntryColumns.FieldEntryColumn
import dataDictionary.ObjectRow.Countries.Country
import dataDictionary.ObjectRow.StorageTypes.{HdfsAvro, StorageType}
import dataDictionary.ObjectRow.StorageZones.{RawData, StorageZone}

case class FieldEntriesObject(fieldEntries: Seq[FieldEntry]) {

  def merge(fieldEntriesObject: FieldEntriesObject, columnsArgumentHasPrecedence: Iterable[FieldEntryColumn] = Seq()): FieldEntriesObject = {
    //todo multiple source fields with different source objects, etc.
    val definedSourceFieldToFieldEntryArgument = fieldEntriesObject.fieldEntries.filter(_.sourceField.isDefined).groupBy(_.sourceField.get).mapValues(_.head)
    FieldEntriesObject(fieldEntries.map(x => x.sourceField.map(definedSourceFieldToFieldEntryArgument.get(_).map(x.merge(_, columnsArgumentHasPrecedence)).getOrElse(x)).getOrElse(x)))
  }


  def nEntries: Int = {
    fieldEntries.length
  }


  def physicalNameObject: Option[String] = {
    Some(fieldEntries.flatMap(_.physicalNameObject).distinct).filter(_.lengthCompare(1) == 0).map(_.head)
  }


  def withCountry(country: Country): FieldEntriesObject = {
    copy(fieldEntries.map(_.copy(country = Some(country))))
  }


  def withCountryTheConceptualEntity(countryTheConceptualEntity: Country): FieldEntriesObject = {
    copy(fieldEntries.map(_.copy(countryTheConceptualEntity = Some(countryTheConceptualEntity))))
  }


  def withKey(physicalNameFields: Iterable[String]): FieldEntriesObject = {
    FieldEntriesObject(fieldEntries.map(x => if(x.physicalNameField.exists(y => physicalNameFields.exists(_.equalsIgnoreCase(y)))) x.copy(key = Some(FieldRowBooleans.Yes), defaultValue = None) else x.copy(key = Some(FieldRowBooleans.No))))
  }


  def withMandatory(mandatory: FieldRowBoolean): FieldEntriesObject = {
    FieldEntriesObject(fieldEntries.map(_.copy(mandatory = Some(mandatory))))
  }


  def withPhysicalNameSourceObject(physicalNameSourceObject: String): FieldEntriesObject = {
    copy(fieldEntries.map(_.copy(physicalNameSourceObject = Some(physicalNameSourceObject))))
  }


  def withRawFromTextValues: FieldEntriesObject = {
    this.withStorageType(HdfsAvro).withStorageZone(RawData).withDataTypeSourceField(DataTypes.string).withDataType(DataTypes.string).withMandatory(FieldRowBooleans.No)
  }


  def withRegistrationDates: FieldEntriesObject = {
    copy(fieldEntries.map(x => if(x.registrationDate.isEmpty) x.copy(registrationDate = Some(LocalDate.now())) else x))
  }


  def withoutRegistrationDates: FieldEntriesObject = {
    copy(fieldEntries.map(_.copy(registrationDate = None)))
  }


  def withTrustedDataSource(trustedDataSource: FieldRowBoolean): FieldEntriesObject = {
    copy(fieldEntries.map(_.copy(tds = Some(trustedDataSource))))
  }


  private def withDataType(dataType: String): FieldEntriesObject = {
    copy(fieldEntries.map(_.copy(dataType = Some(dataType))))
  }


  private def withDataTypeSourceField(dataTypeSourceField: String): FieldEntriesObject = {
    copy(fieldEntries.map(_.copy(dataTypeSourceField = Some(dataTypeSourceField))))
  }


  private def withStorageType(storageType: StorageType): FieldEntriesObject = {
    copy(fieldEntries.map(_.copy(storageType = Some(storageType))))
  }


  private def withStorageZone(storageZone: StorageZone): FieldEntriesObject = {
    copy(fieldEntries.map(_.copy(storageZone = Some(storageZone))))
  }

}
