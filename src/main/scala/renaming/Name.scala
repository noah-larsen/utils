package renaming

trait Name {

  def name: String


  lazy val normalizedSubstrings: Seq[String] = Name.normalizedSubstrings(name)

}

object Name {

  def normalizedSubstrings(name: String): Seq[String] = {
    val separator = "_"
    name.split(separator).map(_.toLowerCase)
  }

}