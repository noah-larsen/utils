package utils.commands

import utils.enumerated.EnumeratedType
import utils.enumerated.EnumeratedType.NameFormats.{CaseFormats, ObjectNameWithSpacesBetweenWords}

abstract class Command(
                        val letterName: Option[Char],
                        val parameters: Seq[Parameter] = Seq()
                      ) extends EnumeratedType(ObjectNameWithSpacesBetweenWords(CaseFormats.Lowercase)) {

  def usage: String = {
    val descriptionParametersSeparator = ": "
    val parametersSeparator = " "
    val listParameterSuffix = " ..."
    val countingNumberRangedNamedCommandDescriptionSuffix = s" $countingNumberRangedNamedCommandUsageSymbol"
    s"${letterName.getOrElse(countingNumberRangedNamedCommandUsageSymbol)} - $name${letterName.map(_ => new String).getOrElse(countingNumberRangedNamedCommandDescriptionSuffix)}${parameters.headOption.map(_ => descriptionParametersSeparator + parameters
      .map(x => x.name + (if(x.isList) listParameterSuffix else new String)).mkString(parametersSeparator)).getOrElse(new String)}"
  }


  private val countingNumberRangedNamedCommandUsageSymbol = "#"

}
