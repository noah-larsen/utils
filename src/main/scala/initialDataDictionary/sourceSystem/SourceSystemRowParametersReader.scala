package initialDataDictionary.sourceSystem

import dataDictionary.Type
import dataDictionary.enumerations.YesOrNoValues
import dataDictionary.enumerations.YesOrNoValues.Yes
import dataDictionary.enumerations.{Countries, FileTypes, Frequencies, LoadingTypes}
import dataDictionary.types.LogicalFormats
import googleSpreadsheets.RowParametersReader.{RowParameter, RowParameters}
import googleSpreadsheets.{RowParametersReader, SheetRange}
import initialDataDictionary.enumerations.{DataSuperTypes, MoveExistingPrimaryDateFieldValues, ObjectTypes, TargetStorageSuperTypes}
import initialDataDictionary.sourceSystem.SourceSystem.SourceSystemRowParameters
import initialDataDictionary.sourceSystem.SourceSystem.SourceSystemRowParameters._

import scala.util.Try


object SourceSystemRowParametersReader extends RowParametersReader[SourceSystem] {

  override def sheetRange: SheetRange = {
    SheetRange("Source System", maxColumn)
  }


  override protected def rowParameters: RowParametersReader.RowParameters = SourceSystemRowParameters


  override protected def transform(parameterToValue: Map[RowParameter, String], parameterToValues: Map[RowParameter, Seq[String]]): SourceSystem = {
    initialDataDictionary.sourceSystem.SourceSystem(
      addedPrimaryDateFieldDateFormat = parameterToValue(AddedPrimaryDateFieldDateFormat),
      addedPrimaryDateFieldIndex = Try(parameterToValue(AddedPrimaryDateFieldIndex).toInt).toOption,
      addedPrimaryDateFieldLogicalFormat = Type.logicalFormat(parameterToValue(AddedPrimaryDateFieldLogicalFormat)),
      additionalOperationalSourceSystems = parameterToValues(AdditionalOperationalSourceSystems),
      defaultCountryTheConceptualEntity = Countries.withName(parameterToValue(DefaultCountryOfTheConceptualEntity)),
      defaultCountryTheDataSource = Countries.withName(parameterToValue(DefaultCountryOfTheDataSource)),
      defaultDataSource = parameterToValue(DefaultDataSource),
      defaultDataSuperType = DataSuperTypes.withName(parameterToValue(DefaultDataSuperType)),
      defaultDateFormat = parameterToValue(DefaultDateFormat),
      defaultExtractionFileType = FileTypes.withName(parameterToValue(DefaultExtractionFileType)),
      defaultExtractionFileDelimeter = parameterToValue(DefaultExtractionFileDelimeter),
      defaultFrequency = Frequencies.withName(parameterToValue(DefaultFrequency)),
      defaultIsCore = YesOrNoValues.withName(parameterToValue(DefaultIsCore)).map(YesOrNoValues.toBoolean),
      defaultIsTDS = YesOrNoValues.withName(parameterToValue(DefaultIsTDS)).map(_ == Yes),
      defaultLoadingType = LoadingTypes.withName(parameterToValue(DefaultLoadingType)),
      defaultMailbox = parameterToValue(DefaultMailbox),
      defaultObjectType = ObjectTypes.withName(parameterToValue(DefaultObjectType)),
      defaultPartitions = parameterToValues(DefaultPartitions),
      defaultRawToMasterSchemasPath = parameterToValue(DefaultRawToMasterSchemasPath),
      defaultStagingToRawSchemasPath = parameterToValue(DefaultStagingToRawSchemasPath),
      defaultSystemCodeUUAA = parameterToValue(DefaultSystemCodeUUAA),
      defaultTargetStorageSuperType = TargetStorageSuperTypes.withName(parameterToValue(DefaultTargetStorageSuperType)),
      defaultTimestampFormat = parameterToValue(DefaultTimestampFormat),
      moveExistingPrimaryDateField = MoveExistingPrimaryDateFieldValues.withName(parameterToValue(MoveExistingPrimaryDateField)),
      sourceSystem = parameterToValue(SourceSystemRowParameters.SourceSystem)
    )
  }

}
