package utils.commands

import utils.commands.Commands.{CommandException, IncorrectNumberParametersException, UnknownCommandException}

import scala.io.StdIn
import scala.util.{Failure, Try}

trait Commands {

  type CommandType <: Command


  def promptUntilParsed(leadWithNewlineInitial: Boolean = true, leadWithNewlineSubsequent: Boolean = true): CommandInvocation = {
    println((if(leadWithNewlineInitial) System.lineSeparator() else new String) + usage)
    def readLineUntilNonEmpty: String = StdIn.readLine() match {case x if x.trim.isEmpty => readLineUntilNonEmpty case x => x}
    parse(readLineUntilNonEmpty).recover{case e: CommandException =>
      println(e.message.trim + System.lineSeparator())
      if(leadWithNewlineSubsequent) print(System.lineSeparator())
      promptUntilParsed(leadWithNewlineSubsequent, leadWithNewlineSubsequent)
    }.get
  }


  def parse(line: String): Try[CommandInvocation] = {
    val whitespaceRe = "\\s+"
    val tokens = line.split(whitespaceRe)
    commands.find(_.name == tokens.head)
      .map(x => CommandInvocation(x, tokens.tail).validate)
      .getOrElse(Failure[CommandInvocation](UnknownCommandException))
  }


  def usage: String = {
    commands.map(_.usage).mkString(System.lineSeparator())
  }


  def usageWithLeadingNewline: String = {
    System.lineSeparator() + usage
  }


  protected def commands: Seq[CommandType]


  private[utils] case class CommandInvocation(
                                               command: CommandType,
                                               arguments: Seq[String]
                                             ){

    def validate: Try[CommandInvocation] = {
      this match{
        case _ if !(command.parameters.length to (if(command.parameters.exists(_.isList)) Int.MaxValue else command.parameters.length)).contains(arguments.length) => Failure(IncorrectNumberParametersException)
        case x => Try(x)
      }
    }

  }

}

object Commands {

  case class CommandException(message: String) extends RuntimeException(message)
  object IncorrectNumberParametersException extends CommandException("Incorrect Number of Parameters")
  object UnknownCommandException extends CommandException("Unknown Command")

}
