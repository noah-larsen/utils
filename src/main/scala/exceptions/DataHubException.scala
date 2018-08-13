package exceptions

class DataHubException(
                        message: String,
                        cause: Option[Throwable] = None
                      ) extends Exception(message, cause.orNull)

object DataHubException {

  def apply(message: String, cause: Option[Throwable] = None): DataHubException = {
    new DataHubException(message, cause)
  }

}
