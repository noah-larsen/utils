package utils.commands

import utils.enumerated.SelfNamed
import utils.enumerated.SelfNamed.NameFormats.CaseFormats.FirstLetterLowercase

import scala.util.Try

sealed abstract class AbstractParameter extends SelfNamed(FirstLetterLowercase) {

}

object AbstractParameter {

  abstract class Parameter[T](
                               val parse: String => Try[T] = (x: String) => Try(x.asInstanceOf[T]),
                               val default: Option[T] = None
                             ) extends AbstractParameter {

  }


  abstract class ListParameter[T](
                                   val parse: Seq[String] => Try[Seq[T]] = (x: Seq[String]) => Try(x.map(_.asInstanceOf[T]))
                                 ) extends AbstractParameter {



  }


}
