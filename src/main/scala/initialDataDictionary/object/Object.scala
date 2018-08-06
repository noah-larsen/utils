package initialDataDictionary.`object`

import dataDictionary.ObjectRow.Countries.Country
import dataDictionary.ObjectRow.Frequencies.Frequency
import dataDictionary.ObjectRow.LoadingTypes.LoadingType
import googleSpreadsheets.{GoogleSpreadsheet, Row}
import initialDataDictionary.enumerations.DataSuperTypes.DataSuperType
import initialDataDictionary.enumerations.ObjectTypes.ObjectType
import initialDataDictionary.enumerations.TargetStorageSuperTypes.TargetStorageSuperType
import initialDataDictionary.sourceSystem.SourceSystem

import scala.util.Try

case class Object(
                   businessEngineeringStewardComplete: Boolean,
                   dataOwnerComplete: Boolean,
                   dataArchitectureComplete: Boolean,
                   ingestionComplete: Boolean,
                   objectName: String,
                   logicalName: String,
                   description: String,
                   frequency: Option[Frequency],
                   loadingType: Option[LoadingType],
                   mailbox: String,
                   objectType: Option[ObjectType],
                   sourceOperational: String,
                   dataSuperType: Option[DataSuperType],
                   isIngestedFromFixedWidthTextFiles: Option[Boolean],
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
                   stagingPath: String,
                   rawPath: String,
                   masterPath: String,
                   targetStorageSuperType: Option[TargetStorageSuperType],
                   partitions: Seq[String],
                   stagingSchemaPath: String,
                   rawSchemaPath: String,
                   masterSchemaPath: String
                 ) extends Row {

}

object Object {

  def apply(googleSpreadsheetId: String, sourceSystem: SourceSystem): Try[Seq[Object]] = {
    GoogleSpreadsheet(googleSpreadsheetId).flatMap(_.get(ObjectRowReader(sourceSystem)))
  }

}
