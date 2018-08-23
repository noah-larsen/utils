package dataDictionary.`object`

import java.time.format.DateTimeFormatter

import dataDictionary.Constants
import googleSpreadsheets.{Column, Columns}

object ObjectEntryColumns extends Columns {

  override type RowType = ObjectEntry
  override type ColumnType = ObjectEntryColumn
  sealed abstract class ObjectEntryColumn(val string: ObjectEntry => String) extends Column[ObjectEntry]

  object CountryTheDataSource extends ObjectEntryColumn(x => selfNamedOptionToString(x.countryTheDataSource))
  object PhysicalNameObject extends ObjectEntryColumn(_.physicalNameObject)
  object LogicalNameObject extends ObjectEntryColumn(_.logicalNameObject)
  object DescriptionObject extends ObjectEntryColumn(_.descriptionObject)
  object InformationGroupLevel1 extends ObjectEntryColumn(_.informationGroupLevel1)
  object InformationGroupLevel2 extends ObjectEntryColumn(_.informationGroupLevel2)
  object InformationGroupLevel3 extends ObjectEntryColumn(_.informationGroupLevel3)
  object Core extends ObjectEntryColumn(x => selfNamedOptionToString(x.core))
  object Perimeter extends ObjectEntryColumn(_.perimeter)
  object InformationLevel extends ObjectEntryColumn(_.informationLevel)
  object DataSource extends ObjectEntryColumn(_.dataSource)
  object TechnicalResponsible extends ObjectEntryColumn(_.technicalResponsible)
  object StorageType extends ObjectEntryColumn(x => selfNamedOptionToString(x.storageType))
  object StorageZone extends ObjectEntryColumn(x => selfNamedOptionToString(x.storageZone))
  object ObjectType extends ObjectEntryColumn(_.objectType)
  object PhysicalPath extends ObjectEntryColumn(_.physicalPath)
  object Schema extends ObjectEntryColumn(_.schema)
  object SystemCodeUuaa extends ObjectEntryColumn(_.systemCodeUuaa)
  object Partitions extends ObjectEntryColumn(_.partitions.mkString(Constants.listSeparator))
  object Frequency extends ObjectEntryColumn(x => selfNamedOptionToString(x.frequency))
  object TimeRequirement extends ObjectEntryColumn(_.timeRequirement)
  object LoadingType extends ObjectEntryColumn(x => selfNamedOptionToString(x.loadingType))
  object CurrentDepth extends ObjectEntryColumn(_.currentDepth)
  object RequiredDepth extends ObjectEntryColumn(_.requiredDepth)
  object EstimatedVolumeRecords extends ObjectEntryColumn(_.estimatedVolumeRecords)
  object SourceOperational extends ObjectEntryColumn(_.sourceOperational)
  object SourceSystem extends ObjectEntryColumn(_.sourceSystem)
  object PhysicalNameSourceObject extends ObjectEntryColumn(_.physicalNameSourceObject)
  object MailboxSourceTable extends ObjectEntryColumn(_.mailboxSourceTable)
  object SourcePath extends ObjectEntryColumn(_.sourcePath)
  object SchemaPath extends ObjectEntryColumn(_.schemaPath)
  object SourceFileType extends ObjectEntryColumn(x => selfNamedOptionToString(x.sourceFileType))
  object SourceFileDelimeter extends ObjectEntryColumn(_.sourceFileDelimeter)
  object TargetFileType extends ObjectEntryColumn(x => selfNamedOptionToString(x.targetFileType))
  object TargetFileDelimeter extends ObjectEntryColumn(_.targetFileDelimeter)
  object RegistrationDate extends ObjectEntryColumn(_.registrationDate.map(_.format(DateTimeFormatter.ofPattern(Constants.registrationDateFormat))).getOrElse(new String))
  object Tags extends ObjectEntryColumn(_.tags.mkString(Constants.listSeparator))


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[ObjectEntryColumns.type], classOf[ObjectEntryColumn])

}