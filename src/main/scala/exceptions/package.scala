package object exceptions {

  case class FinalDataDictionaryNotFound(sourceSystem: String) extends DataHubException(s"Final Data Dictionary Not Found: $sourceSystem")
  case class InitialDataDictionaryNotFound(sourceSystem: String) extends DataHubException(s"Initial Data Dictionary Not Found: $sourceSystem")
  case class IntermediateDataDictionaryAlreadyContainsEntriesForObject() extends DataHubException("Intermediate Data Dictionary Already Contains Entries For Object")
  case class ObjectContainsDuplicateFieldNames(objectName: String) extends DataHubException(s"Object Contains Duplicate Field Names: $objectName")
  case class ObjectNotFoundInInitialDataDictionary(objectName: String) extends DataHubException(s"Object Not Found In Initial Data Dictionary: $objectName")
  case class InformationSetsToMergeContainIncompatibleFields() extends DataHubException("Information Sets To Merge Contain Incompatible Fields")
  case class WorkDocumentEntriesForObjectDoNotExist(objectName: String) extends DataHubException(s"Work Document Entries For Object Do Not Exist: $objectName")

}
