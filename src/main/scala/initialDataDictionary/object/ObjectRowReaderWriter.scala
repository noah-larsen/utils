package initialDataDictionary.`object`

import dataDictionary.enumerations.Countries.Country
import dataDictionary.enumerations.FileTypes.FileType
import dataDictionary.enumerations.Frequencies.Frequency
import dataDictionary.enumerations.LoadingTypes.LoadingType
import dataDictionary.enumerations.YesOrNoValues
import dataDictionary.enumerations.{Countries, FileTypes, Frequencies, LoadingTypes}
import googleSpreadsheets._
import initialDataDictionary.ObjectAndFields
import initialDataDictionary.`object`.ObjectRowReaderWriter.ObjectRowReaderWriterColumns
import initialDataDictionary.enumerations.DataSuperTypes.DataSuperType
import initialDataDictionary.enumerations.ObjectTypes.ObjectType
import initialDataDictionary.enumerations.TargetStorageSuperTypes.TargetStorageSuperType
import initialDataDictionary.enumerations.{DataSuperTypes, ObjectTypes, TargetStorageSuperTypes}
import initialDataDictionary.sourceSystem.SourceSystem
import utils.enumerated.{Enumerated, SelfNamed}

import scala.util.matching.Regex

case class ObjectRowReaderWriter(sourceSystemForReading: SourceSystem) extends RowReaderWriter[Object_] {

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
      frequency = withDefaultIfEmpty(r(8), Frequencies.withName(_), sourceSystemForReading.defaultFrequency),
      mailbox = withDefaultIfEmpty(r(9), sourceSystemForReading.defaultMailbox),
      sourceOperational = withDefaultIfEmpty(r(10), sourceSystemForReading.sourceSystem),
      extractionFileType = withDefaultIfEmpty(r(11), FileTypes.withName(_), sourceSystemForReading.defaultExtractionFileType),
      extractionFileDelimeter = withDefaultIfEmpty(r(12), sourceSystemForReading.defaultExtractionFileDelimeter),
      objectType = withDefaultIfEmpty(r(13), ObjectTypes.withName(_), sourceSystemForReading.defaultObjectType),
      dataSuperType = withDefaultIfEmpty(r(14), DataSuperTypes.withName(_), sourceSystemForReading.defaultDataSuperType),
      isIngestedFromFixedWidth = withDefaultIfEmpty(r(15), YesOrNoValues.toBooleanOption, sourceSystemForReading.defaultIsIngestedFromFixedWidth),
      currentDepth = r(16),
      perimeter = r(17),
      informationLevel = r(18),
      estimatedVolumeRecords = r(19),
      technicalResponsible = r(20),
      informationGroup = Some(r(21).split(ObjectRowReaderWriter.informationGroupLevelsSeparator).map(_.trim)).filter(_.length == ObjectRowReaderWriter.nInformationGroupLevels).map{case Array(x, y, z) => (x, y, z)},
      isCore = withDefaultIfEmpty(r(22), YesOrNoValues.toBooleanOption, sourceSystemForReading.defaultIsCore),
      isTDS = withDefaultIfEmpty(r(23), YesOrNoValues.toBooleanOption, sourceSystemForReading.defaultIsTDS),
      countryTheDataSource = withDefaultIfEmpty(r(24), Countries.withName(_), sourceSystemForReading.defaultCountryTheDataSource),
      timeRequirement = r(25),
      requiredDepth = r(26),
      systemCodeUUAA = withDefaultIfEmpty(r(27), sourceSystemForReading.defaultSystemCodeUUAA),
      dataSource = withDefaultIfEmpty(r(28), sourceSystemForReading.defaultDataSource),
      stagingPath = r(29),
      rawPath = r(30),
      masterPath = r(31),
      targetStorageSuperType = withDefaultIfEmpty(r(32), TargetStorageSuperTypes.withName(_), sourceSystemForReading.defaultTargetStorageSuperType),
      partitions = withDefaultIfEmpty(r(33), _.split(Regex.quote(ObjectRowReaderWriter.partitionsSeparator)), sourceSystemForReading.defaultPartitions),
      stagingToRawSchemasPath = withDefaultIfEmpty(r(34), sourceSystemForReading.defaultStagingToRawSchemasPath),
      rawToMasterSchemasPath = withDefaultIfEmpty(r(35), sourceSystemForReading.defaultRawToMasterSchemasPath)
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


  object ObjectRowReaderWriterColumns extends Columns {

    override type RowType = Object_
    override type ColumnType = ObjectColumn

    sealed abstract class ObjectColumn(val string: Object_ => String) extends Column[Object_]

    object BusinessEngineeringStewardComplete extends ObjectColumn(_.businessEngineeringStewardComplete.toString)
    object DataOwnerComplete extends ObjectColumn(_.dataOwnerComplete.toString)
    object DataArchitectureComplete extends ObjectColumn(_.dataArchitectureComplete.toString)
    object IngestionComplete extends ObjectColumn(_.ingestionComplete.toString)
    object ObjectName extends ObjectColumn(_.objectName)
    object LogicalName extends ObjectColumn(_.logicalName)
    object Description extends ObjectColumn(_.description)
    object LoadingType extends ObjectColumn(x => selfNamedOptionToString(x.loadingType))
    object Frequency extends ObjectColumn(x => selfNamedOptionToString(x.frequency))
    object Mailbox extends ObjectColumn(_.mailbox)
    object SourceOperational extends ObjectColumn(_.sourceOperational)
    object ExtractionFileType extends ObjectColumn(x => selfNamedOptionToString(x.extractionFileType))
    object ExtractionFileDelimeter extends ObjectColumn(_.extractionFileDelimeter)
    object ObjectType extends ObjectColumn(x => selfNamedOptionToString(x.objectType))
    object DataSuperType extends ObjectColumn(x => selfNamedOptionToString(x.dataSuperType))
    object IsIngestedFromFixedWidth extends ObjectColumn(x => anyValOptionToString(x.isIngestedFromFixedWidth))
    object CurrentDepth extends ObjectColumn(_.currentDepth)
    object Perimeter extends ObjectColumn(_.perimeter)
    object InformationLevel extends ObjectColumn(_.informationLevel)
    object EstimatedVolumeRecords extends ObjectColumn(_.estimatedVolumeRecords)
    object TechnicalResponsible extends ObjectColumn(_.technicalResponsible)
    object InformationGroup extends ObjectColumn(_.informationGroup.map(x => Seq(x._1, x._2, x._3).mkString(generalSeparator + informationGroupLevelsSeparator + generalSeparator)).getOrElse(new String))
    object IsCore extends ObjectColumn(x => anyValOptionToString(x.isCore))
    object IsTDS extends ObjectColumn(x => anyValOptionToString(x.isTDS))
    object CountryTheDataSource extends ObjectColumn(x => selfNamedOptionToString(x.countryTheDataSource))
    object TimeRequirement extends ObjectColumn(_.timeRequirement)
    object RequiredDepth extends ObjectColumn(_.requiredDepth)
    object SystemCodeUUAA extends ObjectColumn(_.systemCodeUUAA)
    object DataSource extends ObjectColumn(_.dataSource)
    object StagingPath extends ObjectColumn(_.stagingPath)
    object RawPath extends ObjectColumn(_.rawPath)
    object MasterPath extends ObjectColumn(_.masterPath)
    object TargetStorageSuperType extends ObjectColumn(x => selfNamedOptionToString(x.targetStorageSuperType))
    object Partitions extends ObjectColumn(_.partitions.mkString(partitionsSeparator))
    object StagingToRawSchemasPath extends ObjectColumn(_.stagingToRawSchemasPath)
    object RawToMasterSchemasPath extends ObjectColumn(_.rawToMasterSchemasPath)
    

    override val values = Seq(BusinessEngineeringStewardComplete, DataOwnerComplete, DataArchitectureComplete, IngestionComplete, ObjectName, LogicalName, Description, LoadingType, Frequency, Mailbox, SourceOperational, ExtractionFileType,
      ExtractionFileDelimeter, ObjectType, DataSuperType, IsIngestedFromFixedWidth, CurrentDepth, Perimeter, InformationLevel, EstimatedVolumeRecords, TechnicalResponsible, InformationGroup, IsCore, IsTDS, CountryTheDataSource, TimeRequirement, RequiredDepth,
      SystemCodeUUAA, DataSource, StagingPath, RawPath, MasterPath, TargetStorageSuperType, Partitions, StagingToRawSchemasPath, RawToMasterSchemasPath)

  }


  private val informationGroupLevelsSeparator = "//"
  private val generalSeparator = " "
  private val nInformationGroupLevels = 3
  private val partitionsSeparator = ";"

}