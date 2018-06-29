package utils

trait AbstractCommand {

  def name: String


  def parameters: Seq[String]


  def description: String = {
    descriptionFromClassName
  }


  def usage: String = {
    s"$name - $description${parameters.headOption.map(_ => ": " + parameters.mkString(" ")).getOrElse(new String)}"
  }


  private def descriptionFromClassName: String = {
    val wordSeparator = " "
    val multipleClassNameAddedDisambiguationSymbol = "$"
    val classNameSeparatorRE = "[.$]"
    getClass.getName.split(classNameSeparatorRE).filter(_.nonEmpty).last.zipWithIndex.flatMap(y => Some(Unit).filter(_ => y._1.isUpper && y._2 != 0).map(_ => wordSeparator).getOrElse(new String) + y._1.toLower).mkString
  }

}
