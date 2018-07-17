package utils.commands

import utils.commands.Commands.{CommandException, IncorrectNumberParametersException, UnknownCommandException}

import scala.io.StdIn
import scala.util.{Failure, Try}

trait Commands {

  type CommandType <: Command


  def promptUntilParsed(nCountingNumberNamedCommands: Int = 0, leadWithNewlineInitial: Boolean = true, leadWithNewlineSubsequent: Boolean = true): CommandInvocation = {
    println((if(leadWithNewlineInitial) System.lineSeparator() else new String) + usage)
    def readLineUntilNonEmpty: String = StdIn.readLine() match {case x if x.trim.isEmpty => readLineUntilNonEmpty case x => x}
    parse(readLineUntilNonEmpty, nCountingNumberNamedCommands).recover{case e: CommandException =>
      println(e.message.trim + System.lineSeparator())
      if(leadWithNewlineSubsequent) print(System.lineSeparator())
      promptUntilParsed(nCountingNumberNamedCommands, leadWithNewlineSubsequent, leadWithNewlineSubsequent)
    }.get
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
                                               arguments: Seq[String],
                                               countingNumberNamedCommandN: Option[Int] = None
                                             ){

    def validate: Try[CommandInvocation] = {
      this match{
        case _ if !(command.parameters.length to (if(command.parameters.exists(_.isList)) Int.MaxValue else command.parameters.length)).contains(arguments.length) => Failure(IncorrectNumberParametersException)
        case x => Try(x)
      }
    }

  }


  private def parse(line: String, nCountingNumberNamedCommands: Int = 0): Try[CommandInvocation] = {
    val whitespaceRe = "\\s+"
    val tokens = line.split(whitespaceRe)
    commands
      .find(x => !x.representsCountingNumberNamedCommands && x.name == tokens.head).map(x => CommandInvocation(x, tokens.tail).validate)
      .orElse(commands.find(_.representsCountingNumberNamedCommands).map((_, Try(tokens.head.toInt))).filter(_._2.isSuccess).map(x => (x._1, x._2.get)).filter(x => x._2 > 0 && x._2 <= nCountingNumberNamedCommands).map(x => CommandInvocation(x._1,
        tokens.tail, Some(x._2)).validate))
      .getOrElse(Failure[CommandInvocation](UnknownCommandException))
  }

}

object Commands {

  case class CommandException(message: String) extends RuntimeException(message)
  object IncorrectNumberParametersException extends CommandException("Incorrect Number of Parameters")
  object UnknownCommandException extends CommandException("Unknown Command")

}
