package initialDataDictionary.`object`

import dataDictionary.enumerations.Countries.Country
import dataDictionary.enumerations.FileTypes.FileType
import dataDictionary.enumerations.Frequencies.Frequency
import dataDictionary.enumerations.LoadingTypes.LoadingType
import googleSpreadsheets.{GoogleSpreadsheet, Row}
import initialDataDictionary.enumerations.DataSuperTypes.DataSuperType
import initialDataDictionary.enumerations.ObjectTypes.ObjectType
import initialDataDictionary.enumerations.TargetStorageSuperTypes.TargetStorageSuperType
import initialDataDictionary.sourceSystem.SourceSystem

import scala.util.Try

case class Object_(
                    businessEngineeringStewardComplete: Boolean,
                    dataOwnerComplete: Boolean,
                    dataArchitectureComplete: Boolean,
                    ingestionComplete: Boolean,
                    objectName: String,
                    logicalName: String,
                    description: String,
                    loadingType: Option[LoadingType],
                    frequency: Option[Frequency],
                    mailbox: String,
                    sourceOperational: String,
                    extractionFileType: Option[FileType],
                    extractionFileDelimeter: String,
                    objectType: Option[ObjectType],
                    dataSuperType: Option[DataSuperType],
                    isIngestedFromFixedWidth: Option[Boolean],
                    currentDepth: String,
                    perimeter: String,
                    informationLevel: String,
                    estimatedVolumeRecords: String,
                    technicalResponsible: String,
                    informationGroup: Option[(String, String, String)],
                    isCore: Option[Boolean],
                    isTDS: Option[Boolean],
                    countryTheDataSource: Option[Country],
                    timeRequirement: String,
                    requiredDepth: String,
                    systemCodeUUAA: String,
                    dataSource: String,
                    stagingPath: String,
                    rawPath: String,
                    masterPath: String,
                    targetStorageSuperType: Option[TargetStorageSuperType],
                    partitions: Seq[String],
                    stagingToRawSchemasPath: String,
                    rawToMasterSchemasPath: String
                 ) extends Row {

}

