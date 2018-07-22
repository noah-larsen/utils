package utils.commands

import utils.enumerated.EnumeratedType
import utils.enumerated.EnumeratedType.NameFormats.{CaseFormats, ObjectNameWithSpacesBetweenWords}

abstract class AbstractCommand extends EnumeratedType(ObjectNameWithSpacesBetweenWords(CaseFormats.Lowercase)) {

  def parameters: Seq[Parameter]


  def usage: String


  protected def usage(nameSymbol: Char): String = {
    val descriptionParametersSeparator = ": "
    val parametersSeparator = " "
    val listParameterSuffix = " ..."
    s"$nameSymbol - $name${parameters.headOption.map(_ => descriptionParametersSeparator + parameters
      .map(x => x.name + (if(x.isList) listParameterSuffix else new String)).mkString(parametersSeparator)).getOrElse(new String)}"
  }

}

