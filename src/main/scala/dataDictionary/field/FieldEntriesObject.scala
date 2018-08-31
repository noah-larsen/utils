package dataDictionary.field

import java.time.LocalDate

import dataDictionary.Type
import dataDictionary.enumerations.Countries.Country
import dataDictionary.enumerations.IngestionStages.Raw
import dataDictionary.enumerations.StorageTypes.StorageType
import dataDictionary.enumerations.StorageZones.StorageZone
import dataDictionary.enumerations.YesOrNoValues.{No, Yes, YesOrNo}
import dataDictionary.enumerations._
import dataDictionary.`object`.ObjectEntry
import dataDictionary.field.FieldEntryColumns.FieldEntryColumn
import dataDictionary.types.LogicalFormats
import dataDictionary.types.bigData.ParquetTypes
import exceptions.InformationSetsToMergeContainIncompatibleFields
import initialDataDictionary.ObjectAndFields
import initialDataDictionary.enumerations.DataSuperTypes.DataSuperType
import initialDataDictionary.enumerations.MoveExistingPrimaryDateFieldValues
import initialDataDictionary.enumerations.MoveExistingPrimaryDateFieldValues.{NotApplicable, ToTheBeginning}
import initialDataDictionary.field.Field
import initialDataDictionary.sourceSystem.SourceSystem
import renaming.Renaming

import scala.util.Try

case class FieldEntriesObject(fieldEntries: Seq[FieldEntry]) {

  def containsDuplicateNames: Boolean = {
    fieldEntries.groupBy(_.physicalNameField.getOrElse(new String).toLowerCase).exists(_._2.lengthCompare(1) > 0)
  }


  def merge(fieldEntriesObject: FieldEntriesObject, columnsArgumentHasPrecedence: Iterable[FieldEntryColumn] = Seq()): FieldEntriesObject = {
    //todo multiple source fields with different source objects, etc.
    val thisFieldEntryToThatFieldEntry = fieldEntries.map(x => (x, fieldEntriesObject.fieldEntries.find(y => if(x.isGenerated) x.physicalNameField.exists(z => y.physicalNameField.exists(_.equalsIgnoreCase(z))) else x.sourceField.exists(z => y.sourceField
      .exists(_.equalsIgnoreCase(z)))))).toMap
    FieldEntriesObject(fieldEntries.map(x => thisFieldEntryToThatFieldEntry(x).map(x.merge(_, columnsArgumentHasPrecedence)).getOrElse(x)))
  }


  def nEntries: Int = {
    fieldEntries.length
  }


  def physicalNameObject: Option[String] = {
    Some(fieldEntries.flatMap(_.physicalNameObject).distinct).filter(_.lengthCompare(1) == 0).map(_.head)
  }


  def toMasterIfFromTextExtraction(lcSourceFieldToDateFormat: Map[String, String], lcGeneratedFieldNameToDateFormat: Map[String, String], lcMandatoryNonKeySourceFields: Set[String]): FieldEntriesObject = {
    FieldEntriesObject(fieldEntries.filter(!_.isFreeField.contains(true)).map{ fieldEntry =>
      val logicalFormat = fieldEntry.logicalFormat.flatMap(Type(_, LogicalFormats).flatMap(_.logicalFormat))
      val format = logicalFormat.map{
        case Type(LogicalFormats.Decimal, Some(x), y) => FieldEntry.decimalFormat(x, y)
        case _ if fieldEntry.isDateOrTimestamp.contains(true) && fieldEntry.generatedField.contains(FieldGeneratedValues.Yes) => fieldEntry.physicalNameField.flatMap(x => lcGeneratedFieldNameToDateFormat.get(x.toLowerCase)).getOrElse(new String)
        case _ if fieldEntry.isDateOrTimestamp.contains(true) => fieldEntry.sourceField.flatMap(x => lcSourceFieldToDateFormat.get(x.toLowerCase)).getOrElse(new String)
        case _ => new String
      }
      fieldEntry.copy(
        storageType = fieldEntry.storageType.filter(_ == StorageTypes.HdfsAvro).map(_ => StorageTypes.HdfsParquet),
        storageZone = Some(StorageZones.MasterData),
        dataType = logicalFormat.map(ParquetTypes.fromLogicalFormat(_, !fieldEntry.isGenerated).asString),
        format = format,
        mandatory = if(fieldEntry.isKey.contains(true) || fieldEntry.sourceField.exists(x => lcMandatoryNonKeySourceFields.contains(x.toLowerCase))) Some(Yes) else fieldEntry.mandatory,
        physicalNameSourceObject = physicalNameObject,
        sourceField = fieldEntry.physicalNameField,
        dataTypeSourceField = fieldEntry.dataType,
        formatSourceField = fieldEntry.format,
        fieldPositionInTheObject = Some(None),
        generatedField = None
      )
    })
  }


  def withRegistrationDates: FieldEntriesObject = {
    copy(fieldEntries.map(x => if(x.registrationDate.isEmpty) x.copy(registrationDate = Some(LocalDate.now())) else x))
  }


  def withRegistrationDate(registrationDate: LocalDate): FieldEntriesObject = {
    copy(fieldEntries.map(_.copy(registrationDate = Some(registrationDate))))
  }


  def withoutRegistrationDates: FieldEntriesObject = {
    copy(fieldEntries.map(_.copy(registrationDate = None)))
  }

}

object FieldEntriesObject {

  def rawFEOFromTextExtraction(objectAndFields: ObjectAndFields, generatedFields: Seq[GeneratedField], primaryDateFieldTemplate: Option[PrimaryDateFieldTemplate]): FieldEntriesObject = {

    def rawFieldEntryFromTextExtractedField(field: Field, rawObjectEntry: ObjectEntry, dataSuperType: Option[DataSuperType], fieldToLength: Option[Map[Field, Int]]): FieldEntry = {
      FieldEntry(
        country = rawObjectEntry.countryTheDataSource,
        physicalNameObject = Some(rawObjectEntry.physicalNameObject),
        storageType = rawObjectEntry.storageType,
        storageZone = rawObjectEntry.storageZone,
        physicalNameField = Some(new String),
        logicalNameField = Some(field.logicalName),
        simpleFieldDescription = Some(field.description),
        catalog = Some(field.catalog),
        dataType = Some(DataTypes.string),
        format = Some(new String),
        logicalFormat = dataSuperType.flatMap(Type.logicalFormat(field.dataType, _).map(_.asString.toUpperCase)),
        key = Some(YesOrNoValues.from(field.isKey)),
        mandatory = Some(YesOrNoValues.No),
        defaultValue = Some(field.defaultValue),
        physicalNameSourceObject = Some(field.objectName.toLowerCase),
        sourceField = Some(field.fieldName.toLowerCase),
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


    def rawFieldEntryFromGeneratedField(generatedField: GeneratedField, rawObjectEntry: ObjectEntry): FieldEntry = {
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
        logicalFormat = Some(generatedField.logicalFormat.asString.toUpperCase),
        key = Some(No),
        mandatory = Some(Yes),
        defaultValue = Some(generatedField.defaultValue),
        physicalNameSourceObject = Some(new String),
        sourceField = Some(new String),
        dataTypeSourceField = Some(new String),
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


    def rawFieldEntryFromPrimaryDateFieldTemplate(primaryDateFieldTemplate: PrimaryDateFieldTemplate, sourceSystem: SourceSystem, rawObjectEntry: ObjectEntry): FieldEntry = {
      FieldEntry(
        country = rawObjectEntry.countryTheDataSource,
        physicalNameObject = Some(rawObjectEntry.physicalNameObject),
        storageType = rawObjectEntry.storageType,
        storageZone = rawObjectEntry.storageZone,
        physicalNameField = Some(primaryDateFieldTemplate.name),
        logicalNameField = Some(primaryDateFieldTemplate.logicalName),
        simpleFieldDescription = Some(primaryDateFieldTemplate.description),
        catalog = None,
        dataType = Some(DataTypes.string),
        format = Some(new String),
        logicalFormat = Some(sourceSystem.addedPrimaryDateFieldLogicalFormat.get.asString.toUpperCase),
        key = Some(No),
        mandatory = Some(No),
        defaultValue = Some(primaryDateFieldTemplate.defaultValue),
        physicalNameSourceObject = Some(new String),
        sourceField = Some(new String),
        dataTypeSourceField = Some(DataTypes.string),
        formatSourceField = Some(new String),
        tags = Some(rawObjectEntry.tags.take(1)),
        fieldPositionInTheObject = None,
        generatedField = None,
        tokenizationType = Some(new String),
        registrationDate = None,
        countryTheConceptualEntity = primaryDateFieldTemplate.countryTheConceptualEntity,
        conceptualEntity = Some(primaryDateFieldTemplate.conceptualEntity),
        operationalEntity = Some(primaryDateFieldTemplate.operationalEntity),
        tds = Some(YesOrNoValues.from(primaryDateFieldTemplate.isTrustedDataSource))
      )
    }


    val obj = objectAndFields.obj
    val sourceSystem = objectAndFields.sourceSystem
    val rawObjectEntry = ObjectEntry(obj, objectAndFields.sourceSystem, Raw)
    val fieldToLength = Some(objectAndFields.fields).filter(_.forall(_.length.isDefined)).map(x => Some(x.zip(x.map(_.length.get).init.+:(0))).map(y => y.tail.scanLeft((y.head, 1))((z, w) => (w, z._2 + w._2))).get.map(y => (y._1._1, y._2)).toMap)
    val iddDefinedFieldToFieldEntry = objectAndFields.fields.map(x => (x, rawFieldEntryFromTextExtractedField(x, rawObjectEntry, obj.dataSuperType, fieldToLength)))
    val existingPrimaryDateFieldEntry = primaryDateFieldTemplate.flatMap(x => iddDefinedFieldToFieldEntry.find(y => y._1.isPrimaryDateField && y._2.sourceField.isDefined)).map(_._2)
    val iddDefinedFieldEntries = existingPrimaryDateFieldEntry.map(x => Some(Renaming(iddDefinedFieldToFieldEntry.map(_._2)).name(x.sourceField.get, primaryDateFieldTemplate.get.name)).map(entriesWithRenaming =>
      objectAndFields.sourceSystem.moveExistingPrimaryDateField.map{
        case MoveExistingPrimaryDateFieldValues.No => entriesWithRenaming
        case NotApplicable => entriesWithRenaming
        case ToTheBeginning => entriesWithRenaming.moveToFront(primaryDateFieldTemplate.get.name)
      }.getOrElse(entriesWithRenaming)).get.fieldEntries).getOrElse(iddDefinedFieldToFieldEntry.map(_._2))
    val (generatedFieldsAtBeginning, generatedFieldsAtEnd) = generatedFields.partition(_.addedAtBeginning)
    val withIddDefinedAndGeneratedFEs = FieldEntriesObject(generatedFieldsAtBeginning.map(rawFieldEntryFromGeneratedField(_, rawObjectEntry)) ++ iddDefinedFieldEntries ++ generatedFieldsAtEnd.map(rawFieldEntryFromGeneratedField(_, rawObjectEntry)))
    val index = sourceSystem.addedPrimaryDateFieldIndex.get match {
      case x if x > 0 => Math.min(x - 1, withIddDefinedAndGeneratedFEs.fieldEntries.length)
      case x if x < 0 => Math.max(withIddDefinedAndGeneratedFEs.fieldEntries.length + x - 1, 0)
    }
    primaryDateFieldTemplate.filter(_ => existingPrimaryDateFieldEntry.isEmpty).map(x => Renaming(withIddDefinedAndGeneratedFEs).insert(rawFieldEntryFromPrimaryDateFieldTemplate(x, objectAndFields.sourceSystem, rawObjectEntry), index)).getOrElse(
      withIddDefinedAndGeneratedFEs)

  }

}
