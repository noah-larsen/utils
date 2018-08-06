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
      frequency = Frequencies.withName(r(7)),
      loadingType = LoadingTypes.withName(r(8)),
      mailbox = r(9),
      objectType = withDefaultIfNonEmpty(r(10), ObjectTypes.withName(_), sourceSystem.defaultObjectType),
      sourceOperational = Some(r(11)).filter(_.nonEmpty).getOrElse(sourceSystem.sourceSystem),
      withDefaultIfNonEmpty(r(12), DataSuperTypes.withName(_), sourceSystem.defaultDataSuperType),
      isIngestedFromFixedWidthTextFiles = withDefaultIfNonEmpty(r(13), YesOrNoValues.toBooleanOption, sourceSystem.defaultIsIngestedFromFixedWidthTextFiles),
      currentDepth = r(14),
      perimeter = r(15),
      informationLevel = r(16),
      estimatedVolumeRecords = r(17),
      technicalResponsible = r(18),
      informationGroup = Some(r(19).split(informationGroupLevelsSeparator).map(_.trim)).filter(_.length == nInformationGroupLevels).map{case Array(x, y, z) => (x, y, z)},
      isCore = withDefaultIfNonEmpty(r(20), YesOrNoValues.toBooleanOption, sourceSystem.defaultIsCore),
      isTDS = withDefaultIfNonEmpty(r(21), YesOrNoValues.toBooleanOption, sourceSystem.defaultIsTDS),
      withDefaultIfNonEmpty(r(22), Countries.withName(_), sourceSystem.defaultCountryTheDataSource),
      timeRequirement = r(23),
      requiredDepth = r(24),
      systemCodeUUAA = Some(r(25)).filter(_.nonEmpty).getOrElse(sourceSystem.defaultSystemCodeUUAA),
      stagingPath = r(26),
      rawPath = r(27),
      masterPath = r(28),
      withDefaultIfNonEmpty(r(32), TargetStorageSuperTypes.withName(_), sourceSystem.defaultTargetStorageSuperType),
      partitions = r(33).split(Regex.quote(partitionsSeparator)),
      stagingSchemaPath = Some(r(34)).filter(_.nonEmpty).getOrElse(sourceSystem.defaultStagingSchemaPath),
      rawSchemaPath = Some(r(35)).filter(_.nonEmpty).getOrElse(sourceSystem.defaultRawSchemaPath),
      masterSchemaPath = Some(r(36)).filter(_.nonEmpty).getOrElse(sourceSystem.defaultMasterSchemaPath)
    )
  }


  def withDefaultIfNonEmpty[T](value: String, transform: String => T, default: T): T = {
    if(value.isEmpty) default else transform(value)
  }


  private val informationGroupLevelsSeparator = "//"
  private val nInformationGroupLevels = 3
  private val partitionsSeparator = ";"

}
