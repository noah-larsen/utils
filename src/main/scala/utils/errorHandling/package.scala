package utils

import scala.util.{Failure, Try}

package object errorHandling {

  def invert[T](seq: Seq[Try[T]]): Try[Seq[T]] = {
    seq.find(_.isFailure).map(x => Failure(x.failed.get)).getOrElse(Try(seq.map(_.get)))
  }

}
