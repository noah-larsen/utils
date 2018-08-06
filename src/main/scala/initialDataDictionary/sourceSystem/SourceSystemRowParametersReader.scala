package initialDataDictionary.sourceSystem

import dataDictionary.FieldEntry.YesOrNoValues
import dataDictionary.FieldEntry.YesOrNoValues.Yes
import dataDictionary.ObjectRow.{Countries, FileTypes}
import dataDictionary.PhysicalNameObject.SourceTypes
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
      defaultCountryTheDataSource = Countries.withName(parameterToValue(DefaultCountryOfTheDataSource)),
      defaultDataSuperType = DataSuperTypes.withName(parameterToValue(DefaultDataSuperType)),
      defaultExtractionFileType = FileTypes.withName(parameterToValue(DefaultExtractionFileType)),
      defaultExtractionFileDelimeter = parameterToValue(DefaultExtractionFileDelimeter),
      defaultIsCore = YesOrNoValues.withName(parameterToValue(DefaultIsIngestedFromFixedWidthTextFiles)).map(YesOrNoValues.toBoolean),
      defaultIsIngestedFromFixedWidthTextFiles = YesOrNoValues.withName(parameterToValue(DefaultIsIngestedFromFixedWidthTextFiles)).map(_ == Yes),
      defaultIsTDS = YesOrNoValues.withName(parameterToValue(DefaultIsTDS)).map(_ == Yes),
      defaultMasterSchemaPath = parameterToValue(DefaultMasterSchemaPath),
      defaultObjectType = ObjectTypes.withName(parameterToValue(DefaultObjectType)),
      defaultPartitions = parameterToValues(DefaultPartitions),
      defaultRawSchemaPath = parameterToValue(DefaultRawSchemaPath),
      defaultStagingSchemaPath = parameterToValue(DefaultStagingSchemaPath),
      defaultSystemCodeUUAA = parameterToValue(DefaultSystemCodeUUAA),
      defaultTargetStorageSuperType = TargetStorageSuperTypes.withName(parameterToValue(DefaultTargetStorageSuperType)),
      sourceSystem = parameterToValue(SourceSystemRowParameters.SourceSystem)
    )
  }

}