package utils.commands

import utils.commands.AbstractParameter.{ListParameter, Parameter}
import utils.commands.Commands.{CommandException, IncorrectNumberParametersException, InvalidParametersException, UnknownCommandException}
import utils.enumerated.Enumerated

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

trait Commands extends Enumerated {

  type CommandType <: Command
  override type T = CommandType


  def promptUntilParsed[T](translatedOneBasedIndexCommandItems: Seq[T] = Seq(), showUsage: Boolean = true, leadWithNewline: Boolean = true): CommandInvocation[T] = {
    if(showUsage) println((if(leadWithNewline) System.lineSeparator() else new String) + usage)
    def readLineUntilNonEmpty: String = StdIn.readLine() match {case x if Option(x).forall(_.trim.isEmpty) => readLineUntilNonEmpty case x => x}
    parse(readLineUntilNonEmpty, translatedOneBasedIndexCommandItems).recover{case e: CommandException =>
      println(e.message.trim + System.lineSeparator())
      promptUntilParsed(translatedOneBasedIndexCommandItems, showUsage = false)
    }.get
  }


  def usage: String = {
    oneBasedIndexCommand.map(Seq(_)).getOrElse(Nil).++(letterCommands).map(_.usage).mkString(System.lineSeparator())
  }


  def usageWithLeadingNewline: String = {
    System.lineSeparator() + usage
  }


  protected def letterCommands: Seq[CommandType] = {
    values.filter(!_.isInstanceOf[OneBasedIndexCommand])
  }


  protected def oneBasedIndexCommand: Option[CommandType with OneBasedIndexCommand] = {
    values.collectFirst{case x: Command with OneBasedIndexCommand => x.asInstanceOf[CommandType with OneBasedIndexCommand]}
  }


  private[utils] case class CommandInvocation[T](
                                                  command: CommandType,
                                                  arguments: Seq[String],
                                                  oneBasedIndexCommandSelection: Option[T] = None
                                                ){

    def validate: Try[CommandInvocation[T]] = {
      Unit match {
        case _ if !(parameters.length - command.defaultParameters.length to parameters.collectFirst{case _: ListParameter[_] => Int.MaxValue}.getOrElse(parameters.length))
          .contains(arguments.length) => Failure(IncorrectNumberParametersException)
          //todo list isValid
        case _ if parameterToArguments.exists(x => x._1 match {
          case y: ListParameter[_] => y.parse(x._2).isFailure
          case y: Parameter[_] => y.parse(x._2.head).isFailure
        }) => Failure(InvalidParametersException)
        case _ => Try(this)
      }
    }


    def value[U](listParameter: ListParameter[U]): Seq[U] = {
      listParameter.parse(arguments.slice(command.parameters.indexOf(listParameter), arguments.length)).get
    }


    def value[U](parameter: Parameter[U]): U = {
      (parameters.indexOf(parameter), parameter.default) match {
        case (x, Some(y)) if !arguments.indices.contains(x) => y
        case (x, _) => parameter.parse(arguments(x)).get
      }
    }


    private def parameterToArguments: Map[AbstractParameter, Seq[String]] = {
      parameters.lastOption.map{
        case x: ListParameter[_] => parameters.init.zip(arguments).map(x => (x._1, Seq(x._2))).toMap + (x -> arguments.slice(parameters.length - 1, arguments.length))
        case x: Parameter[_] => parameters.zip(arguments).map(x => (x._1, Seq(x._2))).toMap
      }.getOrElse(Map())
    }


    private def parameters: Seq[AbstractParameter] = {
      command.parameters
    }

  }


  private def parse[T](line: String, translatedOneBasedIndexCommandItems: Seq[T] = Seq()): Try[CommandInvocation[T]] = {
    val whitespaceRe = "\\s+"
    val tokens = line.split(whitespaceRe)
    letterCommands
      .find(_.letterName.toString == tokens.head).map(CommandInvocation[T](_, tokens.tail).validate)
      .orElse(oneBasedIndexCommand.map((_, Try(tokens.head.toInt))).filter(_._2.isSuccess).map(x => (x._1, x._2.get)).filter(x => x._2 > 0 && x._2 <=
        translatedOneBasedIndexCommandItems.length).map(x => CommandInvocation(x._1, tokens.tail, Some(translatedOneBasedIndexCommandItems(x._2 - 1))).validate))
      .getOrElse(Failure[CommandInvocation[T]](UnknownCommandException))
  }

}

object Commands {

  case class CommandException(message: String) extends RuntimeException(message)
  object IncorrectNumberParametersException extends CommandException("Incorrect Number of Parameters")
  object InvalidParametersException extends CommandException("Invalid Parameters")
  object UnknownCommandException extends CommandException("Unknown Command")

}
