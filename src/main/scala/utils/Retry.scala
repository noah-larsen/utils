package utils

import scala.util.{Failure, Try}

object Retry {

  //todo add delay
  def apply[T](f: => Try[T], maxNAttempts: Int = Int.MaxValue, uponRetry: PartialFunction[Exception, Unit] = {case _ =>}): Try[T] = {
    maxNAttempts match {
      case 1 => f
      case x if x > 1 => f.recoverWith{
        case e: Exception =>
          uponRetry(e)
          apply(f, maxNAttempts - 1, uponRetry)
      }
      case _ => throw new IllegalArgumentException
    }
  }

}
