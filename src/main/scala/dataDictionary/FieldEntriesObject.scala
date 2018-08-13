package dataDictionary

import java.time.LocalDate

import consoleApplication.ConsoleRenamer.Languages.Language
import dataDictionary.enumerations.YesOrNoValues.YesOrNo
import dataDictionary.enumerations.{DataTypes, FieldGeneratedValues, YesOrNoValues}
import dataDictionary.FieldEntryReaderWriter.FieldEntryColumns.FieldEntryColumn
import dataDictionary.enumerations.Countries.Country
import dataDictionary.enumerations.IngestionStages.{IngestionStage, Raw}
import dataDictionary.enumerations.StorageTypes.{HdfsAvro, StorageType}
import dataDictionary.enumerations.StorageZones.{RawData, StorageZone}
import dataDictionary.enumerations.{StorageTypes, StorageZones}
import dataDictionary.types.{LogicalFormats, SuperTypes}
import dataDictionary.types.LogicalFormats.LogicalFormat
import dataDictionary.types.bigData.ParquetTypes
import initialDataDictionary.ObjectAndFields

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
      val logicalFormat = fieldEntry.logicalFormat.flatMap(Type(_, LogicalFormats).asInstanceOf[Option[Type[LogicalFormat]]])
      val format = logicalFormat.map{
        case Type(LogicalFormats.Decimal, Some(x), y) => FieldEntry.decimalFormat(x, y)
        case x if Seq(LogicalFormats.Date, LogicalFormats.Time, LogicalFormats.Timestamp).contains(x.typeType) => fieldEntry.sourceField.map(sourceFieldToDateFormat).getOrElse(new String)
        case _ => new String
      }
      fieldEntry.copy(
        storageType = fieldEntry.storageType.filter(_ == StorageTypes.HdfsAvro).map(_ => StorageTypes.HdfsParquet),
        storageZone = Some(StorageZones.MasterData),
        dataType = logicalFormat.map(ParquetTypes.fromLogicalFormat(_).string),
        format = format,
        physicalNameSourceObject = physicalNameObject,
        sourceField = fieldEntry.physicalNameField,
        dataTypeSourceField = fieldEntry.dataType,
        formatSourceField = fieldEntry.format,
        fieldPositionInTheObject = Some(None),
        generatedField = None
      )
    })
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


  def withRawFromTextValues: FieldEntriesObject = {
    this.withStorageType(HdfsAvro).withStorageZone(RawData).withDataTypeSourceField(DataTypes.string).withDataType(DataTypes.string).withMandatory(YesOrNoValues.No)
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

  def rawFEOFromTextExtraction(objectAndFields: ObjectAndFields): FieldEntriesObject = {
    objectAndFields.fields match {
      case x if x.isEmpty => FieldEntriesObject(Seq())
      case fields =>
        val obj = objectAndFields.obj
        val objectEntry = ObjectEntry(obj, objectAndFields.sourceSystem, Raw)
        val fieldToLength = Some(fields).filter(_.forall(_.length.isDefined)).map(x => Some(x.zip(x.map(_.length.get).init.+:(0))).map(y => y.tail.scanLeft((y.head, 1))((z, w) => (w, z._2 + w._2))).get.map(y => (y._1._1, y._2)).toMap)
        //todo physicalNameField assignment assumes no renamings
        FieldEntriesObject(objectAndFields.fields.map(field =>
          FieldEntry(
            country = obj.countryTheDataSource,
            physicalNameObject = Some(objectEntry.physicalNameObject),
            storageType = objectEntry.storageType,
            storageZone = objectEntry.storageZone,
            physicalNameField = Some(field.fieldName),
            logicalNameField = Some(field.logicalName),
            simpleFieldDescription = Some(field.description),
            catalog = Some(field.catalog),
            dataType = Some(DataTypes.string),
            format = Some(new String),
            logicalFormat = obj.dataSuperType.flatMap(Type.logicalFormat(field.dataType, _).map(_.string)),
            key = Some(YesOrNoValues.from(field.isKey)),
            mandatory = Some(YesOrNoValues.from(field.isMandatoryNonKey)),
            defaultValue = Some(field.defaultValue),
            physicalNameSourceObject = Some(field.objectName),
            sourceField = Some(field.fieldName),
            dataTypeSourceField = Some(DataTypes.string),
            formatSourceField = Some(new String),
            tags = Some(Seq(field.objectName)),
            fieldPositionInTheObject = Some(fieldToLength.map(_(field))),
            generatedField = None,
            tokenizationType = Some(field.tokenizationType.map(_.name).getOrElse(new String)),
            registrationDate = None,
            countryTheConceptualEntity = field.countryTheConceptualEntity,
            conceptualEntity = Some(field.conceptualEntity),
            operationalEntity = Some(field.operationalEntity),
            tds = field.isTDS.map(YesOrNoValues.from)
          )
        ))
    }
  }

}
