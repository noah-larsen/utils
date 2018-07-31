package dataDictionary

import java.time.LocalDate

import consoleApplication.ConsoleRenamer.Languages.Language
import dataDictionary.FieldEntry.FieldRowBooleans.FieldRowBoolean
import dataDictionary.FieldEntry.{DataTypes, FieldGeneratedValues, FieldRowBooleans}
import dataDictionary.FieldEntryReaderWriter.FieldEntryColumns.FieldEntryColumn
import dataDictionary.ObjectRow.Countries.Country
import dataDictionary.ObjectRow.{StorageTypes, StorageZones}
import dataDictionary.ObjectRow.StorageTypes.{HdfsAvro, StorageType}
import dataDictionary.ObjectRow.StorageZones.{RawData, StorageZone}
import dataDictionary.types.LogicalFormats
import dataDictionary.types.LogicalFormats.LogicalFormat
import dataDictionary.types.bigData.ParquetTypes
import renaming.TargetName

import scala.util.Try

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


  def toMaster: FieldEntriesObject = {
    FieldEntriesObject(fieldEntries.filter(!_.isFreeField.contains(true)).map(fieldEntry => fieldEntry.copy(
      storageType = fieldEntry.storageType.filter(_ == StorageTypes.HdfsAvro).map(_ => StorageTypes.HdfsParquet),
      storageZone = Some(StorageZones.MasterData),
      dataType = fieldEntry.logicalFormat.flatMap(Type(_, LogicalFormats).asInstanceOf[Option[Type[LogicalFormat]]].map(ParquetTypes.fromLogicalFormat(_).string)),
      physicalNameSourceObject = physicalNameObject,
      sourceField = fieldEntry.physicalNameField,
      dataTypeSourceField = fieldEntry.dataType,
      formatSourceField = fieldEntry.format,
      fieldPositionInTheObject = Some(None),
      generatedField = None
    )))
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
