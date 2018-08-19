package utils.commands

import utils.commands.Parameter.{ListParameter, OptionalParameter, ValueParameter}
import utils.commands.Commands.{CommandException, IncorrectNumberParametersException, InvalidParametersException, UnknownCommandException}
import utils.enumerated.Enumerated

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

trait Commands extends Enumerated {

  type CommandType <: Command
  override type T = CommandType


  def promptUntilParsed[T](translatedOneBasedIndexCommandItems: Seq[T] = Seq(), listParameterToRuntimeValidationF: Map[ListParameter[_], Seq[String] => Try[Unit]] = Map(),
                           showUsage: Boolean = true, leadWithNewline: Boolean = true)(implicit clearScreenUponSuccess: Boolean = true): CommandInvocation[T] = {

    def clearScreen(): Unit ={
      val nBlankLines = 100
      println(System.lineSeparator() * nBlankLines)
    }


    if(showUsage) println((if(leadWithNewline) System.lineSeparator() else new String) + usage)
    def readLineUntilNonEmpty: String = StdIn.readLine() match {case x if Option(x).forall(_.trim.isEmpty) => readLineUntilNonEmpty case x => x}
    parse(readLineUntilNonEmpty, translatedOneBasedIndexCommandItems, listParameterToRuntimeValidationF).recover{case e: CommandException =>
      println(e.message.trim + System.lineSeparator())
      promptUntilParsed(translatedOneBasedIndexCommandItems, listParameterToRuntimeValidationF, showUsage = false)
    }.get match {
      case x =>
        if(clearScreenUponSuccess) clearScreen()
        x
    }
    
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
                                                  oneBasedIndexCommandSelection: Option[T] = None,
                                                  oneBasedIndexListCommandSelection: Option[Seq[T]] = None
                                                ){

    def validate(listParameterToRuntimeValidationF: Map[ListParameter[_], Seq[String] => Try[Unit]] = Map()): Try[CommandInvocation[T]] = {
      Unit match {
        case _ if !(parameters.length - command.nUnrequiredParameters to parameters.collectFirst{case _: ListParameter[_] => Int.MaxValue}.getOrElse(parameters.length))
          .contains(arguments.length) => Failure(IncorrectNumberParametersException)
        case _ if parameterToArguments.exists(x => x._1 match {
          case y: ValueParameter[_] => y.parse(x._2.head).isFailure
          case y: OptionalParameter[_] => y.parse(x._2.head).isFailure
          case y: ListParameter[_] => y.parse(x._2).isFailure || listParameterToRuntimeValidationF.get(y).exists(_(x._2).isFailure)
        }) => Failure(InvalidParametersException)
        case _ => Try(this)
      }
    }


    def value[U](valueParameter: ValueParameter[U]): U = {
      (parameters.indexOf(valueParameter), valueParameter.default) match {
        case (x, Some(y)) if !arguments.indices.contains(x) => y
        case (x, _) => valueParameter.parse(arguments(x)).get
      }
    }


    def value[U](optionalParameter: OptionalParameter[U]): Option[U] = {
      Some(parameters.indexOf(optionalParameter))
        .filter(arguments.indices.contains)
        .map(x => optionalParameter.parse(arguments(x)).get)
    }


    def value[U](listParameter: ListParameter[U]): Seq[U] = {
      listParameter.parse(arguments.slice(command.parameters.indexOf(listParameter), arguments.length)).get
    }


    private def parameterToArguments: Map[Parameter, Seq[String]] = {
      parameters.lastOption.map{
        case x: ValueParameter[_] => parameters.zip(arguments).map(x => (x._1, Seq(x._2))).toMap
        case x: OptionalParameter[_] => parameters.zip(arguments).map(x => (x._1, Seq(x._2))).toMap
        case x: ListParameter[_] => parameters.init.zip(arguments).map(x => (x._1, Seq(x._2))).toMap + (x -> arguments.slice(parameters.length - 1, arguments.length))
      }.getOrElse(Map())
    }


    private def parameters: Seq[Parameter] = {
      command.parameters
    }

  }


  private def parse[T](line: String, translatedOneBasedIndexCommandItems: Seq[T] = Seq(),
                       listParameterToRuntimeValidationF: Map[ListParameter[_], Seq[String] => Try[Unit]] = Map()): Try[CommandInvocation[T]] = {

    val whitespaceRe = "\\s+"
    val tokens = line.split(whitespaceRe)


    letterCommands
      .find(_.letterName.toString == tokens.head).map(CommandInvocation[T](_, tokens.tail).validate(listParameterToRuntimeValidationF))
      .orElse(
        oneBasedIndexCommand.collect {
          case _: OneBasedIndexListCommand =>
            oneBasedIndexCommand.map((_, tokens.map(y => Try(y.toInt)))).filter(_._2.forall(_.isSuccess)).map(x => (x._1, x._2.map(_.get))).filter(_._2.forall(x => x > 0 &&
              x <= translatedOneBasedIndexCommandItems.length)).map(x => Try(CommandInvocation[T](x._1, Seq(), None, Some(x._2.map(y => translatedOneBasedIndexCommandItems(y
              - 1))))))
          case _: OneBasedIndexCommand =>
            oneBasedIndexCommand.map((_, Try(tokens.head.toInt))).filter(_._2.isSuccess).map(x => (x._1, x._2.get)).filter(x => x._2 > 0 && x._2 <=
              translatedOneBasedIndexCommandItems.length).map(x => CommandInvocation(x._1, tokens.tail, Some(translatedOneBasedIndexCommandItems(x._2 - 1))).validate(
              listParameterToRuntimeValidationF))
        }.flatten
      )
      .getOrElse(Failure[CommandInvocation[T]](UnknownCommandException))

  }

}

object Commands {

  case class CommandException(message: String) extends RuntimeException(message)
  object IncorrectNumberParametersException extends CommandException("Incorrect Number of Parameters")
  object InvalidParametersException extends CommandException("Invalid Parameters")
  object UnknownCommandException extends CommandException("Unknown Command")

}
