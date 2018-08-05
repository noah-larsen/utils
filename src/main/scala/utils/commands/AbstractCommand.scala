package utils.commands

import utils.commands.AbstractParameter.{ListParameter, Parameter}
import utils.enumerated.SelfNamed
import utils.enumerated.SelfNamed.NameFormats.{CaseFormats, ObjectNameWithSpacesBetweenWords}

abstract class AbstractCommand(parameters_ : Seq[AbstractParameter]) extends SelfNamed(ObjectNameWithSpacesBetweenWords(CaseFormats.Lowercase)) {

  def usage: String


  def defaultParameters: Seq[Parameter[_]] = {
    parameters_.collect{case x: Parameter[_] if x.default.isDefined => x}
  }


  def listParameter: Option[ListParameter[_]] = {
    parameters_.collectFirst{case x: ListParameter[_] => x}
  }


  def nonDefaultParameters: Seq[Parameter[_]] = {
    parameters_.collect{case x: Parameter[_] if x.default.isEmpty => x}
  }


  def parameters: Seq[AbstractParameter] = {
    nonDefaultParameters ++ defaultParameters ++ listParameter.filter(_ => defaultParameters.isEmpty).map(Seq(_)).getOrElse(Nil)
  }


  protected def usage(nameSymbol: Char): String = {
    val descriptionParametersSeparator = ": "
    val parametersSeparator = " "
    val listParameterSuffix = " ..."
    val defaultParameterPrefix = "["
    val defaultParameterNameDefaultValueSeparator = " = "
    val defaultParameterSuffix = "]"
    s"$nameSymbol - $name${parameters.headOption.map(_ => descriptionParametersSeparator + parameters.map{
      case x: Parameter[_] if x.default.isDefined => defaultParameterPrefix + x.name + defaultParameterNameDefaultValueSeparator + x.default.get + defaultParameterSuffix
      case x: Parameter[_] => x.name
      case x: ListParameter[_] => x.name + listParameterSuffix
    }.mkString(parametersSeparator)).getOrElse(new String)}"
  }

}

