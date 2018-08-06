package initialDataDictionary.sourceSystem

import dataDictionary.ObjectRow.Countries.Country
import dataDictionary.ObjectRow.FileTypes.FileType
import dataDictionary.PhysicalNameObject.SourceTypes.SourceType
import googleSpreadsheets.RowParametersReader.{RowParameter, RowParameters}
import googleSpreadsheets.{GoogleSpreadsheet, RowParametersReader, SheetRange}
import initialDataDictionary.enumerations.DataSuperTypes.DataSuperType
import initialDataDictionary.enumerations.ObjectTypes.ObjectType
import initialDataDictionary.enumerations.TargetStorageSuperTypes.TargetStorageSuperType
import utils.enumerated.SelfNamed.NameFormats.{Custom, NameFormat, ObjectNameWithSpacesBetweenWords}

import scala.util.Try

case class SourceSystem(
                         additionalOperationalSourceSystems: Seq[String],
                         defaultCountryTheDataSource: Option[Country],
                         defaultDataSuperType: Option[DataSuperType],
                         defaultExtractionFileType: Option[FileType],
                         defaultExtractionFileDelimeter: String,
                         defaultIsCore: Option[Boolean],
                         defaultIsIngestedFromFixedWidthTextFiles: Option[Boolean],
                         defaultIsTDS: Option[Boolean],
                         defaultMailbox: String,
                         defaultMasterSchemaPath: String,
                         defaultObjectType: Option[ObjectType],
                         defaultPartitions: Seq[String],
                         defaultRawSchemaPath: String,
                         defaultStagingSchemaPath: String,
                         defaultSystemCodeUUAA: String,
                         defaultTargetStorageSuperType: Option[TargetStorageSuperType],
                         sourceSystem: String
                       ) {

}

object SourceSystem {

  def apply(googleSpreadsheetId: String): Try[SourceSystem] = {
    GoogleSpreadsheet(googleSpreadsheetId).flatMap(_.get(SourceSystemRowParametersReader))
  }


  private[sourceSystem] object SourceSystemRowParameters extends RowParameters {

    override type RowParameterType = SourceSystemRowParameter
    sealed abstract class SourceSystemRowParameter(isList: Boolean = false, nameFormat: NameFormat = ObjectNameWithSpacesBetweenWords()) extends RowParameter(isList, nameFormat)

    object SourceSystem extends SourceSystemRowParameter
    object AdditionalOperationalSourceSystems extends SourceSystemRowParameter(true)
    object DefaultCountryOfTheDataSource extends SourceSystemRowParameter
    object DefaultDataSuperType extends SourceSystemRowParameter
    object DefaultExtractionFileType extends SourceSystemRowParameter
    object DefaultExtractionFileDelimeter extends SourceSystemRowParameter
    object DefaultIsCore extends SourceSystemRowParameter
    object DefaultIsIngestedFromFixedWidthTextFiles extends SourceSystemRowParameter
    object DefaultIsTDS extends SourceSystemRowParameter(nameFormat = Custom("Default Is TDS"))
    object DefaultMailbox extends
    object DefaultMasterSchemaPath extends SourceSystemRowParameter
    object DefaultObjectType extends SourceSystemRowParameter
    object DefaultPartitions extends SourceSystemRowParameter(true)
    object DefaultRawSchemaPath extends SourceSystemRowParameter
    object DefaultStagingSchemaPath extends SourceSystemRowParameter
    object DefaultSystemCodeUUAA extends SourceSystemRowParameter(nameFormat = Custom("Default System Code UUAA"))
    object DefaultTargetStorageSuperType extends SourceSystemRowParameter


    override val values = Seq(
      SourceSystem, AdditionalOperationalSourceSystems, DefaultCountryOfTheDataSource, DefaultDataSuperType, DefaultExtractionFileType, DefaultExtractionFileDelimeter, DefaultIsIngestedFromFixedWidthTextFiles, DefaultIsTDS, DefaultMasterSchemaPath,
      DefaultObjectType, DefaultPartitions, DefaultRawSchemaPath, DefaultStagingSchemaPath, DefaultSystemCodeUUAA, DefaultTargetStorageSuperType
    )

  }

}

