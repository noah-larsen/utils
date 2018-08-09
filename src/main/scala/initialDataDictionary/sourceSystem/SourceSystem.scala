package initialDataDictionary.sourceSystem

import dataDictionary.enumerations.Countries.Country
import dataDictionary.enumerations.FileTypes.FileType
import dataDictionary.enumerations.Frequencies.Frequency
import dataDictionary.enumerations.LoadingTypes.LoadingType
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
                         defaultDataSource: String,
                         defaultDataSuperType: Option[DataSuperType],
                         defaultExtractionFileType: Option[FileType],
                         defaultExtractionFileDelimeter: String,
                         defaultFrequency: Option[Frequency],
                         defaultIsCore: Option[Boolean],
                         defaultIsIngestedFromFixedWidth: Option[Boolean],
                         defaultIsTDS: Option[Boolean],
                         defaultLoadingType: Option[LoadingType],
                         defaultMailbox: String,
                         defaultObjectType: Option[ObjectType],
                         defaultPartitions: Seq[String],
                         defaultRawToMasterSchemasPath: String,
                         defaultStagingToRawSchemasPath: String,
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
    object DefaultDataSource extends SourceSystemRowParameter
    object DefaultDataSuperType extends SourceSystemRowParameter
    object DefaultExtractionFileType extends SourceSystemRowParameter
    object DefaultExtractionFileDelimeter extends SourceSystemRowParameter
    object DefaultFrequency extends SourceSystemRowParameter
    object DefaultIsCore extends SourceSystemRowParameter
    object DefaultIsIngestedFromFixedWidth extends SourceSystemRowParameter
    object DefaultIsTDS extends SourceSystemRowParameter(nameFormat = Custom("Default Is TDS"))
    object DefaultLoadingType extends SourceSystemRowParameter
    object DefaultMailbox extends SourceSystemRowParameter
    object DefaultObjectType extends SourceSystemRowParameter
    object DefaultPartitions extends SourceSystemRowParameter(true)
    object DefaultRawToMasterSchemasPath extends SourceSystemRowParameter
    object DefaultStagingToRawSchemasPath extends SourceSystemRowParameter
    object DefaultSystemCodeUUAA extends SourceSystemRowParameter(nameFormat = Custom("Default System Code UUAA"))
    object DefaultTargetStorageSuperType extends SourceSystemRowParameter
    object SourceSystem extends SourceSystemRowParameter


    override val values = Seq(
      AdditionalOperationalSourceSystems, DefaultCountryOfTheConceptualEntity, DefaultCountryOfTheDataSource, DefaultDataSuperType, DefaultDataSource, DefaultExtractionFileType, DefaultExtractionFileDelimeter, DefaultFrequency, DefaultIsCore,
      DefaultIsIngestedFromFixedWidth, DefaultIsTDS, DefaultLoadingType, DefaultMailbox, DefaultObjectType, DefaultPartitions, DefaultRawToMasterSchemasPath, DefaultStagingToRawSchemasPath, DefaultSystemCodeUUAA, DefaultTargetStorageSuperType, SourceSystem
    )

  }

}

