package utils

import utils.Enumerated.EnumeratedType

trait Enumerated {

  type T <: EnumeratedType
  val values: Seq[T]


  def contains(name: String): Boolean = {
    withName(name).isDefined
  }


  def withName(name: String): Option[T] = {
    values.find(_.name.equalsIgnoreCase(name))
  }

}

object Enumerated {

  trait EnumeratedType {

    def name: String

  }

}
