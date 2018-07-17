package utils.enumerated

trait Enumerated {

  type T <: EnumeratedType
  val values: Seq[T]


  def contains(name: String): Boolean = {
    withName(name).isDefined
  }


  def withName(name: String, ignoreCase: Boolean = true): Option[T] = {
    values.find(x => if(ignoreCase) x.name.equalsIgnoreCase(name) else x.name == name)
  }

}
