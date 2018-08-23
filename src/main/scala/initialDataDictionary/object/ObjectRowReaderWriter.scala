package initialDataDictionary.`object`

import dataDictionary.enumerations.YesOrNoValues
import dataDictionary.enumerations.{Countries, FileTypes, Frequencies, LoadingTypes}
import googleSpreadsheets._
import initialDataDictionary.ObjectAndFields
import initialDataDictionary.`object`.ObjectRowReaderWriterColumns._
import initialDataDictionary.enumerations.{DataSuperTypes, ObjectTypes, TargetStorageSuperTypes}
import initialDataDictionary.sourceSystem.SourceSystem

import scala.util.matching.Regex

case class ObjectRowReaderWriter(sourceSystemForReading: SourceSystem) extends RowReaderWriter[Object_] {

  override def sheetRange: SheetRange = {
    SheetRange("Objects", "AK", 4)
  }


  override protected def toRow(row: Map[Column[Object_], String]): Object_ = {
    Object_(
      businessEngineeringStewardComplete = CheckboxValues.toBoolean(row(BusinessEngineeringStewardComplete)),
      dataOwnerComplete = CheckboxValues.toBoolean(row(DataOwnerComplete)),
      dataArchitectureComplete = CheckboxValues.toBoolean(row(DataArchitectureComplete)),
      ingestionComplete = CheckboxValues.toBoolean(row(IngestionComplete)),
      objectName = row(ObjectName),
      logicalName = row(LogicalName),
      description = row(Description),
      loadingType = LoadingTypes.withName(row(LoadingType)),
      frequency = withDefaultIfEmpty(row(Frequency), Frequencies.withName(_), sourceSystemForReading.defaultFrequency),
      mailbox = withDefaultIfEmpty(row(Mailbox), sourceSystemForReading.defaultMailbox),
      sourceOperational = withDefaultIfEmpty(row(SourceOperational), sourceSystemForReading.sourceSystem),
      extractionFileType = withDefaultIfEmpty(row(ExtractionFileType), FileTypes.withName(_), sourceSystemForReading.defaultExtractionFileType),
      extractionFileDelimeter = withDefaultIfEmpty(row(ExtractionFileDelimeter), sourceSystemForReading.defaultExtractionFileDelimeter),
      objectType = withDefaultIfEmpty(row(ObjectType), ObjectTypes.withName(_), sourceSystemForReading.defaultObjectType),
      dataSuperType = withDefaultIfEmpty(row(DataSuperType), DataSuperTypes.withName(_), sourceSystemForReading.defaultDataSuperType),
      currentDepth = row(CurrentDepth),
      perimeter = row(Perimeter),
      informationLevel = row(InformationLevel),
      estimatedVolumeRecords = row(EstimatedVolumeRecords),
      technicalResponsible = row(TechnicalResponsible),
      informationGroup = Some(row(InformationGroup).split(ObjectRowReaderWriter.informationGroupLevelsSeparator).map(_.trim)).filter(_.length == ObjectRowReaderWriter.nInformationGroupLevels).map{case Array(x, y, z) => (x, y, z)},
      isCore = withDefaultIfEmpty(row(IsCore), YesOrNoValues.toBooleanOption, sourceSystemForReading.defaultIsCore),
      isTDS = withDefaultIfEmpty(row(IsTDS), YesOrNoValues.toBooleanOption, sourceSystemForReading.defaultIsTDS),
      countryTheDataSource = withDefaultIfEmpty(row(CountryTheDataSource), Countries.withName(_), sourceSystemForReading.defaultCountryTheDataSource),
      timeRequirement = row(TimeRequirement),
      requiredDepth = row(RequiredDepth),
      systemCodeUUAA = withDefaultIfEmpty(row(SystemCodeUUAA), sourceSystemForReading.defaultSystemCodeUUAA),
      dataSource = withDefaultIfEmpty(row(DataSource), sourceSystemForReading.defaultDataSource),
      stagingPath = row(StagingPath),
      rawPath = row(RawPath),
      masterPath = row(MasterPath),
      targetStorageSuperType = withDefaultIfEmpty(row(TargetStorageSuperType), TargetStorageSuperTypes.withName(_), sourceSystemForReading.defaultTargetStorageSuperType),
      partitions = withDefaultIfEmpty(row(Partitions), _.split(Regex.quote(ObjectRowReaderWriter.partitionsSeparator)), sourceSystemForReading.defaultPartitions),
      stagingToRawSchemasPath = withDefaultIfEmpty(row(StagingToRawSchemasPath), sourceSystemForReading.defaultStagingToRawSchemasPath),
      rawToMasterSchemasPath = withDefaultIfEmpty(row(RawToMasterSchemasPath), sourceSystemForReading.defaultRawToMasterSchemasPath)
    )
  }


  override protected def columns: Seq[Column[Object_]] = {
    ObjectRowReaderWriterColumns.values
  }

}

object ObjectRowReaderWriter {

  def apply(objectsAndFields: Seq[ObjectAndFields]): ObjectRowReaderWriter = {
    ObjectRowReaderWriter(objectsAndFields.headOption.map(_.sourceSystem).getOrElse(SourceSystem.empty))
  }


  private[`object`] val informationGroupLevelsSeparator = "//"
  private[`object`] val generalSeparator = " "
  private[`object`] val nInformationGroupLevels = 3
  private[`object`] val partitionsSeparator = ";"

}