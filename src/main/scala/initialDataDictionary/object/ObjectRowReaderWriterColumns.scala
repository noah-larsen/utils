package initialDataDictionary.`object`

import googleSpreadsheets.{Column, Columns}

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
  object CurrentDepth extends ObjectColumn(_.currentDepth)
  object Perimeter extends ObjectColumn(_.perimeter)
  object InformationLevel extends ObjectColumn(_.informationLevel)
  object EstimatedVolumeRecords extends ObjectColumn(_.estimatedVolumeRecords)
  object TechnicalResponsible extends ObjectColumn(_.technicalResponsible)
  object InformationGroup extends ObjectColumn(_.informationGroup.map(x => Seq(x._1, x._2, x._3).mkString(Some(ObjectRowReaderWriter.generalSeparator).map(x => x + ObjectRowReaderWriter.informationGroupLevelsSeparator + x).get)).getOrElse(new String))
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
  object Partitions extends ObjectColumn(_.partitions.mkString(ObjectRowReaderWriter.partitionsSeparator))
  object StagingToRawSchemasPath extends ObjectColumn(_.stagingToRawSchemasPath)
  object RawToMasterSchemasPath extends ObjectColumn(_.rawToMasterSchemasPath)


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[ObjectRowReaderWriterColumns.type], classOf[ObjectColumn])

}