package initialDataDictionary.`object`

import dataDictionary.FieldEntry.YesOrNoValues
import dataDictionary.ObjectRow.Countries.UnitedStates
import dataDictionary.ObjectRow.{Countries, Frequencies, LoadingTypes}
import googleSpreadsheets.{CheckboxValues, RowReader, SheetRange}
import initialDataDictionary.enumerations.{DataSuperTypes, ObjectTypes, TargetStorageSuperTypes}
import initialDataDictionary.sourceSystem.SourceSystem
import utils.enumerated.{Enumerated, SelfNamed}

import scala.util.matching.Regex

case class ObjectRowReader(sourceSystem: SourceSystem) extends RowReader[Object] {

  override def sheetRange: SheetRange = {
    SheetRange("Objects", "AK", 4)
  }


  override protected def toRow(r: Int => String): Object = {
    Object(
      businessEngineeringStewardComplete = CheckboxValues.toBoolean(r(0)),
      dataOwnerComplete = CheckboxValues.toBoolean(r(1)),
      dataArchitectureComplete = CheckboxValues.toBoolean(r(2)),
      ingestionComplete = CheckboxValues.toBoolean(r(3)),
      objectName = r(4),
      logicalName = r(5),
      description = r(6),
      loadingType = LoadingTypes.withName(r(7)),
      frequency = Frequencies.withName(r(8)),
      mailbox = r(9),
      sourceOperational = withDefaultIfEmpty(r(10), sourceSystem.sourceSystem),
      objectType = withDefaultIfEmpty(r(11), ObjectTypes.withName(_), sourceSystem.defaultObjectType),
      dataSuperType = withDefaultIfEmpty(r(12), DataSuperTypes.withName(_), sourceSystem.defaultDataSuperType),
      isIngestedFromFixedWidth = withDefaultIfEmpty(r(13), YesOrNoValues.toBooleanOption, sourceSystem.defaultIsIngestedFromFixedWidth),
      currentDepth = r(14),
      perimeter = r(15),
      informationLevel = r(16),
      estimatedVolumeRecords = r(17),
      technicalResponsible = r(18),
      informationGroup = Some(r(19).split(informationGroupLevelsSeparator).map(_.trim)).filter(_.length == nInformationGroupLevels).map{case Array(x, y, z) => (x, y, z)},
      isCore = withDefaultIfEmpty(r(20), YesOrNoValues.toBooleanOption, sourceSystem.defaultIsCore),
      isTDS = withDefaultIfEmpty(r(21), YesOrNoValues.toBooleanOption, sourceSystem.defaultIsTDS),
      countryTheDataSource = withDefaultIfEmpty(r(22), Countries.withName(_), sourceSystem.defaultCountryTheDataSource),
      timeRequirement = r(23),
      requiredDepth = r(24),
      systemCodeUUAA = withDefaultIfEmpty(r(25), sourceSystem.defaultSystemCodeUUAA),
      stagingPath = r(26),
      rawPath = r(27),
      masterPath = r(28),
      targetStorageSuperType = withDefaultIfEmpty(r(29), TargetStorageSuperTypes.withName(_), sourceSystem.defaultTargetStorageSuperType),
      partitions = r(30).split(Regex.quote(partitionsSeparator)),
      stagingSchemaPath = withDefaultIfEmpty(r(31), sourceSystem.defaultStagingSchemaPath),
      rawSchemaPath = withDefaultIfEmpty(r(32), sourceSystem.defaultRawSchemaPath),
      masterSchemaPath = withDefaultIfEmpty(r(33), sourceSystem.defaultMasterSchemaPath)
    )
  }


  private val informationGroupLevelsSeparator = "//"
  private val nInformationGroupLevels = 3
  private val partitionsSeparator = ";"

}
