package dataDictionary.`object`

import java.time.format.DateTimeFormatter

import dataDictionary.Constants
import dataDictionary.enumerations.IngestionStages.{IngestionStage, Master, Raw}
import dataDictionary.enumerations._
import googleSpreadsheets.{Column, Columns, RowReaderWriter, SheetRange}

case class ObjectEntryReaderWriter(ingestionStage: IngestionStage) extends RowReaderWriter[ObjectEntry] {

  override def sheetRange: SheetRange = {
    val sheetName = ingestionStage match {
      case Raw => "DC-DD-Object-RAW"
      case Master =>"DC-DD-Object-MASTER"
    }
    SheetRange(sheetName, "AK", 6)
  }


  override protected def columns: Seq[Column[ObjectEntry]] = ObjectColumns.values


  override protected def toRow(r: Int => String): ObjectEntry = {
    ObjectEntry(
      countryTheDataSource = dropDown(Countries, r(0)),
      physicalNameObject = r(1),
      logicalNameObject = r(2),
      descriptionObject = r(3),
      informationGroupLevel1 = r(4),
      informationGroupLevel2 = r(5),
      informationGroupLevel3 = r(6),
      core = dropDown(CoreValues, r(7)),
      perimeter = r(8),
      informationLevel = r(9),
      dataSource = r(10),
      technicalResponsible = r(11),
      storageType = dropDown(StorageTypes, r(12)),
      storageZone = dropDown(StorageZones, r(13)),
      objectType = r(14),
      physicalPath = r(15),
      schema = r(16),
      systemCodeUuaa = r(17),
      partitions = list(r(18), Constants.listSeparator),
      frequency = dropDown(Frequencies, r(19)),
      timeRequirement = r(20),
      loadingType = dropDown(LoadingTypes, r(21)),
      currentDepth = r(22),
      requiredDepth = r(23),
      estimatedVolumeRecords = r(24),
      sourceOperational = r(25),
      sourceSystem = r(26),
      physicalNameSourceObject = r(27),
      mailboxSourceTable = r(28),
      sourcePath = r(29),
      schemaPath = r(30),
      sourceFileType = dropDown(FileTypes, r(31)),
      sourceFileDelimeter = r(32),
      targetFileType = dropDown(FileTypes, r(33)),
      targetFileDelimeter = r(34),
      registrationDate = Some(r(35)).filter(_ != new String).map(x => date(x, Seq(Constants.registrationDateFormat, googleSpreadsheets.googleSpreadsheetModifiedDateFormat)).get),
      tags = list(r(36), Constants.listSeparator)
    )
  }


  private object ObjectColumns extends Columns {

    override type RowType = ObjectEntry
    override type ColumnType = ObjectColumn
    sealed abstract class ObjectColumn(val string: ObjectEntry => String) extends Column[ObjectEntry]

    object CountryTheDataSource extends ObjectColumn(x => selfNamedOptionToString(x.countryTheDataSource))
    object PhysicalNameObject extends ObjectColumn(_.physicalNameObject)
    object LogicalNameObject extends ObjectColumn(_.logicalNameObject)
    object DescriptionObject extends ObjectColumn(_.descriptionObject)
    object InformationGroupLevel1 extends ObjectColumn(_.informationGroupLevel1)
    object InformationGroupLevel2 extends ObjectColumn(_.informationGroupLevel2)
    object InformationGroupLevel3 extends ObjectColumn(_.informationGroupLevel3)
    object Core extends ObjectColumn(x => selfNamedOptionToString(x.core))
    object Perimeter extends ObjectColumn(_.perimeter)
    object InformationLevel extends ObjectColumn(_.informationLevel)
    object DataSource extends ObjectColumn(_.dataSource)
    object TechnicalResponsible extends ObjectColumn(_.technicalResponsible)
    object StorageType extends ObjectColumn(x => selfNamedOptionToString(x.storageType))
    object StorageZone extends ObjectColumn(x => selfNamedOptionToString(x.storageZone))
    object ObjectType extends ObjectColumn(_.objectType)
    object PhysicalPath extends ObjectColumn(_.physicalPath)
    object Schema extends ObjectColumn(_.schema)
    object SystemCodeUuaa extends ObjectColumn(_.systemCodeUuaa)
    object Partitions extends ObjectColumn(_.partitions.mkString(Constants.listSeparator))
    object Frequency extends ObjectColumn(x => selfNamedOptionToString(x.frequency))
    object TimeRequirement extends ObjectColumn(_.timeRequirement)
    object LoadingType extends ObjectColumn(x => selfNamedOptionToString(x.loadingType))
    object CurrentDepth extends ObjectColumn(_.currentDepth)
    object RequiredDepth extends ObjectColumn(_.requiredDepth)
    object EstimatedVolumeRecords extends ObjectColumn(_.estimatedVolumeRecords)
    object SourceOperational extends ObjectColumn(_.sourceOperational)
    object SourceSystem extends ObjectColumn(_.sourceSystem)
    object PhysicalNameSourceObject extends ObjectColumn(_.physicalNameSourceObject)
    object MailboxSourceTable extends ObjectColumn(_.mailboxSourceTable)
    object SourcePath extends ObjectColumn(_.sourcePath)
    object SchemaPath extends ObjectColumn(_.schemaPath)
    object SourceFileType extends ObjectColumn(x => selfNamedOptionToString(x.sourceFileType))
    object SourceFileDelimeter extends ObjectColumn(_.sourceFileDelimeter)
    object TargetFileType extends ObjectColumn(x => selfNamedOptionToString(x.targetFileType))
    object TargetFileDelimeter extends ObjectColumn(_.targetFileDelimeter)
    object RegistrationDate extends ObjectColumn(_.registrationDate.map(_.format(DateTimeFormatter.ofPattern(Constants.registrationDateFormat))).getOrElse(new String))
    object Tags extends ObjectColumn(_.tags.mkString(Constants.listSeparator))


    override val values = Seq(CountryTheDataSource, PhysicalNameObject, LogicalNameObject, DescriptionObject, InformationGroupLevel1, InformationGroupLevel2, InformationGroupLevel3, Core, Perimeter, InformationLevel, DataSource, TechnicalResponsible,
      StorageType, StorageZone, ObjectType, PhysicalPath, Schema, SystemCodeUuaa, Partitions, Frequency, TimeRequirement, LoadingType, CurrentDepth, RequiredDepth, EstimatedVolumeRecords, SourceOperational, SourceSystem, PhysicalNameSourceObject,
      MailboxSourceTable, SourcePath, SchemaPath, SourceFileType, SourceFileDelimeter, TargetFileType, TargetFileDelimeter, RegistrationDate, Tags
    )

  }

}
