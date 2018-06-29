package renaming

case class Name(name: String) {

  lazy val normalizedSubstrings: Seq[String] = {
    val separator = "_"
    name.split(separator).map(_.toLowerCase)
  }

}
