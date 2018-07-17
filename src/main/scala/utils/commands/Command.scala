package utils.commands

abstract class Command(
                        val letterName: Option[Char],
                        val parameters: Seq[Parameter] = Seq()
                      ) {

  def description: String = {
    descriptionFromClassName
  }


  def usage: String = {
    val descriptionParametersSeparator = ": "
    val parametersSeparator = " "
    val listParameterSuffix = " ..."
    val countingNumberRangedNamedCommandDescriptionSuffix = s" $countingNumberRangedNamedCommandUsageSymbol"
    s"${letterName.getOrElse(countingNumberRangedNamedCommandUsageSymbol)} - $description${letterName.map(_ => new String).getOrElse(countingNumberRangedNamedCommandDescriptionSuffix)}${parameters.headOption.map(_ => descriptionParametersSeparator + parameters
      .map(x => x.name + (if(x.isList) listParameterSuffix else new String)).mkString(parametersSeparator)).getOrElse(new String)}"
  }


  private def descriptionFromClassName: String = {
    val wordSeparator = " "
    val multipleClassNameAddedDisambiguationSymbol = "$"
    val classNameSeparatorRE = "[.$]"
    getClass.getName.split(classNameSeparatorRE).filter(_.nonEmpty).last.zipWithIndex.flatMap(y => Some(Unit).filter(_ => y._1.isUpper && y._2 != 0).map(_ => wordSeparator).getOrElse(new String) + y._1.toLower).mkString
  }


  private val countingNumberRangedNamedCommandUsageSymbol = "#"

}
