package utils

trait AbstractCommand {

  def name: String


  def parameters: Seq[String]


  def description: String = {
    descriptionFromClassName
  }


  def usage: String = {
    s"$name - $description${parameters.headOption.map(_ => ": " + parameters.mkString(", ")).getOrElse(new String)}"
  }


  private def descriptionFromClassName: String = {
    val wordSeparator = " "
    val multipleClassNameAddedDisambiguationSymbol = "$"
    Some(this.getClass.getSimpleName).map(x => x.replace(multipleClassNameAddedDisambiguationSymbol, new String).zipWithIndex.flatMap(y => Some(Unit).filter(_ => y._1
      .isUpper && y._2 != 0).map(_ => wordSeparator).getOrElse(new String) + y._1.toLower)).get.mkString
  }

}