package object exceptions {

  case class InitialDataDictionaryNotFoundException(sourceSystem: String) extends DataHubException(s"Initial Data Dictionary Not Found: $sourceSystem")
  object IntermediateDataDictionaryAlreadyContainsEntriesForObject extends DataHubException("Intermediate Data Dictionary Already Contains Entries For Object")

}
