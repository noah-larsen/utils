package dataDictionary.`object`

import java.time.format.DateTimeFormatter

import dataDictionary.Constants
import dataDictionary.`object`.ObjectEntryColumns._
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


  override protected def columns: Seq[Column[ObjectEntry]] = ObjectEntryColumns.values


  override protected def toRow(row: Map[Column[ObjectEntry], String]): ObjectEntry = {
    ObjectEntry(
      countryTheDataSource = dropDown(Countries, row(CountryTheDataSource)),
      physicalNameObject = row(PhysicalNameObject),
      logicalNameObject = row(LogicalNameObject),
      descriptionObject = row(DescriptionObject),
      informationGroupLevel1 = row(InformationGroupLevel1),
      informationGroupLevel2 = row(InformationGroupLevel2),
      informationGroupLevel3 = row(InformationGroupLevel3),
      core = dropDown(CoreValues, row(Core)),
      perimeter = row(Perimeter),
      informationLevel = row(InformationLevel),
      dataSource = row(DataSource),
      technicalResponsible = row(TechnicalResponsible),
      storageType = dropDown(StorageTypes, row(StorageType)),
      storageZone = dropDown(StorageZones, row(StorageZone)),
      objectType = row(ObjectType),
      physicalPath = row(PhysicalPath),
      schema = row(Schema),
      systemCodeUuaa = row(SystemCodeUuaa),
      partitions = list(row(Partitions), Constants.listSeparator),
      frequency = dropDown(Frequencies, row(Frequency)),
      timeRequirement = row(TimeRequirement),
      loadingType = dropDown(LoadingTypes, row(LoadingType)),
      currentDepth = row(CurrentDepth),
      requiredDepth = row(RequiredDepth),
      estimatedVolumeRecords = row(EstimatedVolumeRecords),
      sourceOperational = row(SourceOperational),
      sourceSystem = row(SourceSystem),
      physicalNameSourceObject = row(PhysicalNameSourceObject),
      mailboxSourceTable = row(MailboxSourceTable),
      sourcePath = row(SourcePath),
      schemaPath = row(SchemaPath),
      sourceFileType = dropDown(FileTypes, row(SourceFileType)),
      sourceFileDelimeter = row(SourceFileDelimeter),
      targetFileType = dropDown(FileTypes, row(TargetFileType)),
      targetFileDelimeter = row(TargetFileDelimeter),
      registrationDate = Some(row(RegistrationDate)).filter(_ != new String).map(x => date(x, Seq(Constants.registrationDateFormat, googleSpreadsheets.googleSpreadsheetModifiedDateFormat)).get),
      tags = list(row(Tags), Constants.listSeparator)
    )
  }

}
