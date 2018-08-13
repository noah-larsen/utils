package object exceptions {

  case class InitialDataDictionaryNotFoundException(sourceSystem: String) extends DataHubException(s"Initial Data Dictionary Not Found: $sourceSystem")

}
