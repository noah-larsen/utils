package dataDictionary.field

import java.time.LocalDate

import dataDictionary.Type
import dataDictionary.enumerations.Countries.Country
import dataDictionary.enumerations.IngestionStages.Raw
import dataDictionary.enumerations.StorageTypes.{HdfsAvro, StorageType}
import dataDictionary.enumerations.StorageZones.{RawData, StorageZone}
import dataDictionary.enumerations.YesOrNoValues.{No, Yes, YesOrNo}
import dataDictionary.enumerations._
import dataDictionary.field.FieldEntryReaderWriter.FieldEntryColumns.FieldEntryColumn
import dataDictionary.`object`.ObjectEntry
import dataDictionary.types.LogicalFormats
import dataDictionary.types.LogicalFormats.LogicalFormat
import dataDictionary.types.bigData.ParquetTypes
import initialDataDictionary.ObjectAndFields
import initialDataDictionary.enumerations.DataSuperTypes.DataSuperType
import initialDataDictionary.field.Field

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


  def toMasterIfFromTextExtraction(sourceFieldToDateFormat: Map[String, String]): FieldEntriesObject = {
    FieldEntriesObject(fieldEntries.filter(!_.isFreeField.contains(true)).map{fieldEntry =>
      val logicalFormat = fieldEntry.logicalFormat.flatMap(Type(_, LogicalFormats).flatMap(_.logicalFormat))
      val format = logicalFormat.map{
        case Type(LogicalFormats.Decimal, Some(x), y) => FieldEntry.decimalFormat(x, y)
        case x if Seq(LogicalFormats.Date, LogicalFormats.Timestamp).contains(x.typeType) => fieldEntry.sourceField.map(sourceFieldToDateFormat).getOrElse(new String)
        case _ => new String
      }
      fieldEntry.copy(
        storageType = fieldEntry.storageType.filter(_ == StorageTypes.HdfsAvro).map(_ => StorageTypes.HdfsParquet),
        storageZone = Some(StorageZones.MasterData),
        dataType = logicalFormat.map(ParquetTypes.fromLogicalFormat(_).asString),
        format = format,
        physicalNameSourceObject = physicalNameObject,
        sourceField = fieldEntry.physicalNameField,
        dataTypeSourceField = fieldEntry.dataType,
        formatSourceField = fieldEntry.format,
        fieldPositionInTheObject = Some(None),
        generatedField = None
      )
    })
    ??? //todo have to in some way get date formats for generated fields as well here
  }


  def withCountry(country: Country): FieldEntriesObject = {
    copy(fieldEntries.map(_.copy(country = Some(country))))
  }


  def withCountryTheConceptualEntity(countryTheConceptualEntity: Country): FieldEntriesObject = {
    copy(fieldEntries.map(_.copy(countryTheConceptualEntity = Some(countryTheConceptualEntity))))
  }


  def withKey(physicalNameFields: Iterable[String]): FieldEntriesObject = {
    FieldEntriesObject(fieldEntries.map(x => if(x.physicalNameField.exists(y => physicalNameFields.exists(_.equalsIgnoreCase(y)))) x.copy(key = Some(YesOrNoValues.Yes), defaultValue = None) else x.copy(key = Some(YesOrNoValues.No))))
  }


  def withMandatory(mandatory: YesOrNo): FieldEntriesObject = {
    FieldEntriesObject(fieldEntries.map(_.copy(mandatory = Some(mandatory))))
  }


  def withPhysicalNameSourceObject(physicalNameSourceObject: String): FieldEntriesObject = {
    copy(fieldEntries.map(_.copy(physicalNameSourceObject = Some(physicalNameSourceObject))))
  }


  def withRegistrationDates: FieldEntriesObject = {
    copy(fieldEntries.map(x => if(x.registrationDate.isEmpty) x.copy(registrationDate = Some(LocalDate.now())) else x))
  }


  def withoutRegistrationDates: FieldEntriesObject = {
    copy(fieldEntries.map(_.copy(registrationDate = None)))
  }


  def withTrustedDataSource(trustedDataSource: YesOrNo): FieldEntriesObject = {
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

object FieldEntriesObject {

  def rawFEOFromTextExtraction(objectAndFields: ObjectAndFields, fieldsToGenerate: Seq[GeneratedField]): FieldEntriesObject = {

    def rawFieldEntryFromTextExtractedField(field: Field, rawObjectEntry: ObjectEntry, dataSuperType: Option[DataSuperType], fieldToLength: Option[Map[Field, Int]]): FieldEntry = {
      //todo physicalNameField assignment assumes no renamings
      FieldEntry(
        country = rawObjectEntry.countryTheDataSource,
        physicalNameObject = Some(rawObjectEntry.physicalNameObject),
        storageType = rawObjectEntry.storageType,
        storageZone = rawObjectEntry.storageZone,
        physicalNameField = Some(field.fieldName),
        logicalNameField = Some(field.logicalName),
        simpleFieldDescription = Some(field.description),
        catalog = Some(field.catalog),
        dataType = Some(DataTypes.string),
        format = Some(new String),
        logicalFormat = dataSuperType.flatMap(Type.logicalFormat(field.dataType, _).map(_.asString)),
        key = Some(YesOrNoValues.from(field.isKey)),
        mandatory = Some(YesOrNoValues.from(field.isMandatoryNonKey)),
        defaultValue = Some(field.defaultValue),
        physicalNameSourceObject = Some(field.objectName),
        sourceField = Some(field.fieldName),
        dataTypeSourceField = Some(DataTypes.string),
        formatSourceField = Some(new String),
        tags = Some(rawObjectEntry.tags.take(1)),
        fieldPositionInTheObject = Some(fieldToLength.map(_(field))),
        generatedField = None,
        tokenizationType = Some(field.tokenizationType.map(_.name).getOrElse(new String)),
        registrationDate = None,
        countryTheConceptualEntity = field.countryTheConceptualEntity,
        conceptualEntity = Some(field.conceptualEntity),
        operationalEntity = Some(field.operationalEntity),
        tds = field.isTDS.map(YesOrNoValues.from)
      )
    }


    def rawFieldEntryFromGeneratedTextExtractedField(generatedField: GeneratedField, rawObjectEntry: ObjectEntry): FieldEntry = {
      FieldEntry(
        country = rawObjectEntry.countryTheDataSource,
        physicalNameObject = Some(rawObjectEntry.physicalNameObject),
        storageType = rawObjectEntry.storageType,
        storageZone = rawObjectEntry.storageZone,
        physicalNameField = Some(generatedField.name),
        logicalNameField = Some(generatedField.logicalName),
        simpleFieldDescription = Some(generatedField.description),
        catalog = Some(generatedField.catalog),
        dataType = Some(DataTypes.string),
        format = Some(new String),
        logicalFormat = Some(generatedField.logicalFormat.asString),
        key = Some(No),
        mandatory = Some(Yes),
        defaultValue = Some(generatedField.defaultValue),
        physicalNameSourceObject = Some(new String),
        sourceField = Some(new String),
        dataTypeSourceField = Some(DataTypes.string),
        formatSourceField = Some(new String),
        tags = Some(rawObjectEntry.tags.take(1)),
        fieldPositionInTheObject = None,
        generatedField = Some(FieldGeneratedValues.Yes),
        tokenizationType = Some(generatedField.tokenizationType.map(_.name).getOrElse(new String)),
        registrationDate = None,
        countryTheConceptualEntity = generatedField.countryTheConceptualEntity,
        conceptualEntity = Some(generatedField.conceptualEntity),
        operationalEntity = Some(generatedField.operationalEntity),
        tds = Some(YesOrNoValues.from(generatedField.isTrustedDataSource))
      )
    }


    val obj = objectAndFields.obj
    val rawObjectEntry = ObjectEntry(obj, objectAndFields.sourceSystem, Raw)
    val fieldToLength = Some(objectAndFields.fields).filter(_.forall(_.length.isDefined)).map(x => Some(x.zip(x.map(_.length.get).init.+:(0))).map(y => y.tail.scanLeft((y.head, 1))((z, w) => (w, z._2 + w._2))).get.map(y => (y._1._1, y._2)).toMap)
    val nonGeneratedFieldEntries = objectAndFields.fields.map(rawFieldEntryFromTextExtractedField(_, rawObjectEntry, obj.dataSuperType, fieldToLength))
    val generatedFields = fieldsToGenerate.filter(x => x.generatedIfAlreadyDefinedWithSameLogicalFormat || !nonGeneratedFieldEntries.exists(y => y.sourceField.exists(_.equalsIgnoreCase(x.name)) && y.logicalFormat.exists(_.equalsIgnoreCase(x.logicalFormat.asString))))
    val (generatedFieldsAtBeginning, generatedFieldsAtEnd) = generatedFields.partition(_.generatedAtBeginning)
    FieldEntriesObject(generatedFieldsAtBeginning.map(rawFieldEntryFromGeneratedTextExtractedField(_, rawObjectEntry)) ++ nonGeneratedFieldEntries ++ generatedFieldsAtEnd.map(rawFieldEntryFromGeneratedTextExtractedField(_, rawObjectEntry)))

  }

}
