package initialDataDictionary.sourceSystem

import dataDictionary.enumerations.YesOrNoValues
import dataDictionary.enumerations.YesOrNoValues.Yes
import dataDictionary.enumerations.{Countries, FileTypes, Frequencies, LoadingTypes}
import googleSpreadsheets.RowParametersReader.{RowParameter, RowParameters}
import googleSpreadsheets.{RowParametersReader, SheetRange}
import initialDataDictionary.enumerations.{DataSuperTypes, ObjectTypes, TargetStorageSuperTypes}
import initialDataDictionary.sourceSystem.SourceSystem.SourceSystemRowParameters
import initialDataDictionary.sourceSystem.SourceSystem.SourceSystemRowParameters._


object SourceSystemRowParametersReader extends RowParametersReader[SourceSystem] {

  override def sheetRange: SheetRange = {
    SheetRange("Source System", maxColumn)
  }

  override protected def rowParameters: RowParametersReader.RowParameters = SourceSystemRowParameters


  override protected def transform(parameterToValue: Map[RowParameter, String], parameterToValues: Map[RowParameter, Seq[String]]): SourceSystem = {
    initialDataDictionary.sourceSystem.SourceSystem(
      additionalOperationalSourceSystems = parameterToValues(AdditionalOperationalSourceSystems),
      defaultCountryTheConceptualEntity = Countries.withName(parameterToValue(DefaultCountryOfTheConceptualEntity)),
      defaultCountryTheDataSource = Countries.withName(parameterToValue(DefaultCountryOfTheDataSource)),
      defaultDataSource = parameterToValue(DefaultDataSource),
      defaultDataSuperType = DataSuperTypes.withName(parameterToValue(DefaultDataSuperType)),
      defaultExtractionFileType = FileTypes.withName(parameterToValue(DefaultExtractionFileType)),
      defaultExtractionFileDelimeter = parameterToValue(DefaultExtractionFileDelimeter),
      defaultFrequency = Frequencies.withName(parameterToValue(DefaultFrequency)),
      defaultIsCore = YesOrNoValues.withName(parameterToValue(DefaultIsCore)).map(YesOrNoValues.toBoolean),
      defaultIsIngestedFromFixedWidth = YesOrNoValues.withName(parameterToValue(DefaultIsIngestedFromFixedWidth)).map(_ == Yes),
      defaultIsTDS = YesOrNoValues.withName(parameterToValue(DefaultIsTDS)).map(_ == Yes),
      defaultLoadingType = LoadingTypes.withName(parameterToValue(DefaultLoadingType)),
      defaultMailbox = parameterToValue(DefaultMailbox),
      defaultObjectType = ObjectTypes.withName(parameterToValue(DefaultObjectType)),
      defaultPartitions = parameterToValues(DefaultPartitions),
      defaultRawToMasterSchemasPath = parameterToValue(DefaultRawToMasterSchemasPath),
      defaultStagingToRawSchemasPath = parameterToValue(DefaultStagingToRawSchemasPath),
      defaultSystemCodeUUAA = parameterToValue(DefaultSystemCodeUUAA),
      defaultTargetStorageSuperType = TargetStorageSuperTypes.withName(parameterToValue(DefaultTargetStorageSuperType)),
      sourceSystem = parameterToValue(SourceSystemRowParameters.SourceSystem)
    )
  }

}
