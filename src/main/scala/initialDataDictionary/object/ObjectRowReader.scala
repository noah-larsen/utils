package initialDataDictionary.`object`

import dataDictionary.enumerations.YesOrNoValues
import dataDictionary.enumerations.{Countries, FileTypes, Frequencies, LoadingTypes}
import googleSpreadsheets.{CheckboxValues, RowReader, RowReaderWriter, SheetRange}
import initialDataDictionary.enumerations.{DataSuperTypes, ObjectTypes, TargetStorageSuperTypes}
import initialDataDictionary.sourceSystem.SourceSystem
import utils.enumerated.{Enumerated, SelfNamed}

import scala.util.matching.Regex

case class ObjectRowReader(sourceSystem: SourceSystem) extends RowReader[Object_] {

  override def sheetRange: SheetRange = {
    SheetRange("Objects", "AK", 4)
  }


  override protected def toRow(r: Int => String): Object_ = {
    Object_(
      businessEngineeringStewardComplete = CheckboxValues.toBoolean(r(0)),
      dataOwnerComplete = CheckboxValues.toBoolean(r(1)),
      dataArchitectureComplete = CheckboxValues.toBoolean(r(2)),
      ingestionComplete = CheckboxValues.toBoolean(r(3)),
      objectName = r(4),
      logicalName = r(5),
      description = r(6),
      loadingType = LoadingTypes.withName(r(7)),
      frequency = withDefaultIfEmpty(r(8), Frequencies.withName(_), sourceSystem.defaultFrequency),
      mailbox = withDefaultIfEmpty(r(9), sourceSystem.defaultMailbox),
      sourceOperational = withDefaultIfEmpty(r(10), sourceSystem.sourceSystem),
      extractionFileType = withDefaultIfEmpty(r(11), FileTypes.withName(_), sourceSystem.defaultExtractionFileType),
      extractionFileDelimeter = withDefaultIfEmpty(r(12), sourceSystem.defaultExtractionFileDelimeter),
      objectType = withDefaultIfEmpty(r(13), ObjectTypes.withName(_), sourceSystem.defaultObjectType),
      dataSuperType = withDefaultIfEmpty(r(14), DataSuperTypes.withName(_), sourceSystem.defaultDataSuperType),
      isIngestedFromFixedWidth = withDefaultIfEmpty(r(15), YesOrNoValues.toBooleanOption, sourceSystem.defaultIsIngestedFromFixedWidth),
      currentDepth = r(16),
      perimeter = r(17),
      informationLevel = r(18),
      estimatedVolumeRecords = r(19),
      technicalResponsible = r(20),
      informationGroup = Some(r(21).split(informationGroupLevelsSeparator).map(_.trim)).filter(_.length == nInformationGroupLevels).map{case Array(x, y, z) => (x, y, z)},
      isCore = withDefaultIfEmpty(r(22), YesOrNoValues.toBooleanOption, sourceSystem.defaultIsCore),
      isTDS = withDefaultIfEmpty(r(23), YesOrNoValues.toBooleanOption, sourceSystem.defaultIsTDS),
      countryTheDataSource = withDefaultIfEmpty(r(24), Countries.withName(_), sourceSystem.defaultCountryTheDataSource),
      timeRequirement = r(25),
      requiredDepth = r(26),
      systemCodeUUAA = withDefaultIfEmpty(r(27), sourceSystem.defaultSystemCodeUUAA),
      dataSource = withDefaultIfEmpty(r(28), sourceSystem.defaultDataSource),
      stagingPath = r(29),
      rawPath = r(30),
      masterPath = r(31),
      targetStorageSuperType = withDefaultIfEmpty(r(32), TargetStorageSuperTypes.withName(_), sourceSystem.defaultTargetStorageSuperType),
      partitions = withDefaultIfEmpty(r(33), _.split(Regex.quote(partitionsSeparator)), sourceSystem.defaultPartitions),
      stagingToRawSchemasPath = withDefaultIfEmpty(r(34), sourceSystem.defaultStagingToRawSchemasPath),
      rawToMasterSchemasPath = withDefaultIfEmpty(r(35), sourceSystem.defaultRawToMasterSchemasPath)
    )
  }





  private val informationGroupLevelsSeparator = "//"
  private val nInformationGroupLevels = 3
  private val partitionsSeparator = ";"

}
