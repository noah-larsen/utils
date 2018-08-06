package initialDataDictionary.sourceSystem

import dataDictionary.ObjectRow.Countries.Country
import dataDictionary.ObjectRow.FileTypes.FileType
import dataDictionary.ObjectRow.Frequencies.Frequency
import dataDictionary.ObjectRow.LoadingTypes.LoadingType
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
                         defaultCountryTheConceptualEntity: Option[Country],
                         defaultCountryTheDataSource: Option[Country],
                         defaultDataSuperType: Option[DataSuperType],
                         defaultExtractionFileType: Option[FileType],
                         defaultExtractionFileDelimeter: String,
                         defaultFrequency: Option[Frequency],
                         defaultIsCore: Option[Boolean],
                         defaultIsIngestedFromFixedWidth: Option[Boolean],
                         defaultIsTDS: Option[Boolean],
                         defaultLoadingType: Option[LoadingType],
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

    object AdditionalOperationalSourceSystems extends SourceSystemRowParameter(true)
    object DefaultCountryOfTheConceptualEntity extends SourceSystemRowParameter
    object DefaultCountryOfTheDataSource extends SourceSystemRowParameter
    object DefaultDataSuperType extends SourceSystemRowParameter
    object DefaultExtractionFileType extends SourceSystemRowParameter
    object DefaultExtractionFileDelimeter extends SourceSystemRowParameter
    object DefaultFrequency extends SourceSystemRowParameter
    object DefaultIsCore extends SourceSystemRowParameter
    object DefaultIsIngestedFromFixedWidth extends SourceSystemRowParameter
    object DefaultIsTDS extends SourceSystemRowParameter(nameFormat = Custom("Default Is TDS"))
    object DefaultLoadingType extends SourceSystemRowParameter
    object DefaultMailbox extends SourceSystemRowParameter
    object DefaultMasterSchemaPath extends SourceSystemRowParameter
    object DefaultObjectType extends SourceSystemRowParameter
    object DefaultPartitions extends SourceSystemRowParameter(true)
    object DefaultRawSchemaPath extends SourceSystemRowParameter
    object DefaultStagingSchemaPath extends SourceSystemRowParameter
    object DefaultSystemCodeUUAA extends SourceSystemRowParameter(nameFormat = Custom("Default System Code UUAA"))
    object DefaultTargetStorageSuperType extends SourceSystemRowParameter
    object SourceSystem extends SourceSystemRowParameter


    override val values = Seq(
      AdditionalOperationalSourceSystems, DefaultCountryOfTheConceptualEntity, DefaultCountryOfTheDataSource, DefaultDataSuperType, DefaultExtractionFileType, DefaultExtractionFileDelimeter, DefaultFrequency, DefaultIsCore, DefaultIsIngestedFromFixedWidth,
      DefaultIsTDS, DefaultLoadingType, DefaultMailbox, DefaultMasterSchemaPath, DefaultObjectType, DefaultPartitions, DefaultRawSchemaPath, DefaultStagingSchemaPath, DefaultSystemCodeUUAA, DefaultTargetStorageSuperType, SourceSystem
    )

  }

}

