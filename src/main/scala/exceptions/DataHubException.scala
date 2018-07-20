package exceptions

case class DataHubException(message: String, cause: Option[Throwable] = None) extends Exception(message, cause.orNull)


