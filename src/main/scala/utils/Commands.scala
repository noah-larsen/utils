package utils

import utils.Commands.CommandException

import scala.io.{Source, StdIn}
import scala.util.{Failure, Try}

trait Commands {

  type Command <: AbstractCommand


  def promptUntilParsed(): CommandInvocation = {
    println(System.lineSeparator() + usage)
    parse(StdIn.readLine()).recover{case e: CommandException =>
      println(e.message + System.lineSeparator())
      promptUntilParsed()
    }.get
  }


  def parse(line: String): Try[CommandInvocation] = {
    val whitespace = "\\s+"
    val tokens = line.split(whitespace)
    commands.find(_.name == tokens.head)
      .map(x => Try(CommandInvocation(x, tokens.tail)).filter(_.arguments.lengthCompare(x.parameters.length) == 0).recoverWith{case _: NoSuchElementException => Failure(CommandException("Incorrect Number of Parameters"))})
      .getOrElse(Failure[CommandInvocation](CommandException("Unknown Command")))
  }


  def usage: String = {
    commands.map(_.usage).mkString(System.lineSeparator())
  }


  protected def commands: Seq[Command]


  private[utils] case class CommandInvocation(
                                                       command: Command,
                                                       arguments: Seq[String]
                                                     )

}

object Commands {

  case class CommandException(message: String) extends RuntimeException(message)

}
