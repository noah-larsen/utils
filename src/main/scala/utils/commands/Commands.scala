package utils.commands

import utils.commands.Commands.{CommandException, IncorrectNumberParametersException, UnknownCommandException}

import scala.io.StdIn
import scala.util.{Failure, Try}

trait Commands {

  type CommandType <: Command


  def promptUntilParsed[T](translatedOneBasedIndexCommandItems: Seq[T] = Seq(), showUsage: Boolean = true, leadWithNewLine: Boolean = true): CommandInvocation[T] = {
    if(showUsage) println((if(leadWithNewLine) System.lineSeparator() else new String) + usage)
    def readLineUntilNonEmpty: String = StdIn.readLine() match {case x if x.trim.isEmpty => readLineUntilNonEmpty case x => x}
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


  protected def letterCommands: Seq[CommandType]
  protected def oneBasedIndexCommand: Option[CommandType with OneBasedIndexCommand] = None


  private[utils] case class CommandInvocation[T](
                                                  command: CommandType,
                                                  arguments: Seq[String],
                                                  oneBasedIndexCommandSelection: Option[T] = None
                                                ){

    def validate: Try[CommandInvocation[T]] = {
      this match{
        case _ if !(command.parameters.length to (if(command.parameters.exists(_.isList)) Int.MaxValue else command.parameters.length)).contains(arguments.length) => Failure(IncorrectNumberParametersException)
        case x => Try(x)
      }
    }

  }


  private def parse[T](line: String, translatedOneBasedIndexCommandItems: Seq[T] = Seq()): Try[CommandInvocation[T]] = {
    val whitespaceRe = "\\s+"
    val tokens = line.split(whitespaceRe)
    letterCommands
      .find(_.letterName.toString == tokens.head).map(CommandInvocation[T](_, tokens.tail).validate)
      .orElse(oneBasedIndexCommand.map((_, Try(tokens.head.toInt))).filter(_._2.isSuccess).map(x => (x._1, x._2.get)).filter(x => x._2 > 0 && x._2 <= translatedOneBasedIndexCommandItems.length).map(x => CommandInvocation(x._1, tokens.tail, Some(
        translatedOneBasedIndexCommandItems(x._2 - 1))).validate))
      .getOrElse(Failure[CommandInvocation[T]](UnknownCommandException))
  }

}

object Commands {

  case class CommandException(message: String) extends RuntimeException(message)
  object IncorrectNumberParametersException extends CommandException("Incorrect Number of Parameters")
  object UnknownCommandException extends CommandException("Unknown Command")

}
