package dataDictionary.`object`

import java.time.LocalDate

import dataDictionary.enumerations.CoreValues.CoreValue
import dataDictionary.enumerations.Countries.Country
import dataDictionary.enumerations.FileTypes.{Avro, FileType, Parquet}
import dataDictionary.enumerations.Frequencies.Frequency
import dataDictionary.enumerations.IngestionStages.{IngestionStage, Master, Raw}
import dataDictionary.enumerations.LoadingTypes.LoadingType
import dataDictionary.enumerations.StorageTypes.StorageType
import dataDictionary.enumerations.StorageZones.StorageZone
import dataDictionary.enumerations.{CoreValues, StorageTypes, StorageZones}
import dataDictionary.{PhysicalNameObject, `object`}
import googleSpreadsheets.Row
import initialDataDictionary.`object`.Object_
import initialDataDictionary.enumerations.ObjectTypes
import initialDataDictionary.enumerations.TargetStorageSuperTypes.HdfsAvroParquet
import initialDataDictionary.sourceSystem.SourceSystem

case class ObjectEntry(
                      countryTheDataSource: Option[Country],
                      physicalNameObject: String,
                      logicalNameObject: String,
                      descriptionObject: String,
                      informationGroupLevel1: String,
                      informationGroupLevel2: String,
                      informationGroupLevel3: String,
                      core: Option[CoreValue],
                      perimeter: String,
                      informationLevel: String,
                      dataSource: String,
                      technicalResponsible: String,
                      storageType: Option[StorageType],
                      storageZone: Option[StorageZone],
                      objectType: String,
                      physicalPath: String,
                      schema: String,
                      systemCodeUuaa: String,
                      partitions: Seq[String],
                      frequency: Option[Frequency],
                      timeRequirement: String,
                      loadingType: Option[LoadingType],
                      currentDepth: String,
                      requiredDepth: String,
                      estimatedVolumeRecords: String,
                      sourceOperational: String,
                      sourceSystem: String,
                      physicalNameSourceObject: String,
                      mailboxSourceTable: String,
                      sourcePath: String,
                      schemaPath: String,
                      sourceFileType: Option[FileType],
                      sourceFileDelimeter: String,
                      targetFileType: Option[FileType],
                      targetFileDelimeter: String,
                      tags: Seq[String],
                      registrationDate: Option[LocalDate]
                    ) extends Row {

  def withRegistrationDate: ObjectEntry = {
    if(registrationDate.isDefined) this else copy(registrationDate = Some(LocalDate.now()))
  }

}

object ObjectEntry {

  def apply(obj: Object_, ss: SourceSystem, ingestionStage: IngestionStage): ObjectEntry = {
    //todo error handling what if object type not set?
    //todo everything involving other target storage super types
    val physicalNameObject = PhysicalNameObject(ObjectTypes.toSourceType(obj.objectType.get), obj.systemCodeUUAA, ss.sourceSystem, obj.objectName).asString
    `object`.ObjectEntry(
      countryTheDataSource = obj.countryTheDataSource,
      physicalNameObject = physicalNameObject,
      logicalNameObject = obj.logicalName,
      descriptionObject = obj.description,
      informationGroupLevel1 = obj.informationGroup.map(_._1).getOrElse(new String),
      informationGroupLevel2 = obj.informationGroup.map(_._2).getOrElse(new String),
      informationGroupLevel3 = obj.informationGroup.map(_._3).getOrElse(new String),
      core = obj.isCore.map(CoreValues.from),
      perimeter = obj.perimeter,
      informationLevel = obj.informationLevel,
      dataSource = obj.dataSource,
      technicalResponsible = obj.technicalResponsible,
      storageType = obj.targetStorageSuperType.flatMap(StorageTypes.from(_, ingestionStage)),
      storageZone = Some(StorageZones.from(ingestionStage)),
      objectType = obj.objectType.map(_.name).getOrElse(new String),
      physicalPath = ingestionStage match {case Raw => obj.rawPath case Master => obj.masterPath},
      schema = new String, //todo
      systemCodeUuaa = obj.systemCodeUUAA,
      partitions = obj.partitions,
      frequency = obj.frequency,
      timeRequirement = obj.timeRequirement,
      loadingType = obj.loadingType,
      currentDepth = obj.currentDepth,
      requiredDepth = obj.requiredDepth,
      estimatedVolumeRecords = obj.estimatedVolumeRecords,
      sourceOperational = obj.sourceOperational,
      sourceSystem = ss.sourceSystem,
      physicalNameSourceObject = ingestionStage match {case Raw => obj.objectName case Master => physicalNameObject},
      mailboxSourceTable = obj.mailbox,
      sourcePath = ingestionStage match {case Raw => obj.stagingPath case Master => obj.rawPath},
      schemaPath = ingestionStage match {case Raw => obj.stagingToRawSchemasPath case Master => obj.rawToMasterSchemasPath},
      sourceFileType = ingestionStage match {case Raw => obj.extractionFileType case Master => Some(Avro).filter(_ => obj.targetStorageSuperType.contains(HdfsAvroParquet))},
      sourceFileDelimeter = ingestionStage match {case Raw => obj.extractionFileDelimeter case Master => new String},
      targetFileType = Some(ingestionStage match {case Raw => Avro case Master => Parquet}).filter(_ => obj.targetStorageSuperType.contains(HdfsAvroParquet)),
      targetFileDelimeter = noTargetFileDelimeterValue,
      registrationDate = None,
      tags = Seq(alias(ss.sourceSystem, obj.objectName))
    )
  }


  val partitionsSeparator = ";"


  private val noTargetFileDelimeterValue = "N/A"


  private def alias(sourceSystem: String, sourceObjectName: String): String = {
    val sourceSystemObjectNameSeparator = "_"
    sourceSystem.toLowerCase + sourceSystemObjectNameSeparator + sourceObjectName.toLowerCase
  }

}
