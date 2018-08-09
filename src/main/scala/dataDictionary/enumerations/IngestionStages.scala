package dataDictionary.enumerations

object IngestionStages {
  sealed trait IngestionStage
  object Raw extends IngestionStage
  object Master extends IngestionStage
}