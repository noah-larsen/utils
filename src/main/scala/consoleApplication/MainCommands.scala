package consoleApplication

import consoleApplication.MainCommands.ParameterizedBySourceSystemAndTableName
import utils.commands.{AbstractCommand, Command, Commands, Parameter}

object MainCommands extends Commands {

  override type CommandType = MainCommand
  sealed abstract class MainCommand(parameters: Seq[Parameter] = Seq()) extends Command(parameters)

  object CreateFromInitial extends MainCommand(Seq(Parameter("sourceSystem"), Parameter("tableName"))) with ParameterizedBySourceSystemAndTableName
  object LoadFromIntermediate extends MainCommand(Seq(Parameter("sourceSystem"), Parameter("tableName"))) with ParameterizedBySourceSystemAndTableName
  object WriteOnceToFinal extends MainCommand(Seq(Parameter("sourceSystem"), Parameter("tableName"))) with ParameterizedBySourceSystemAndTableName
  object Quit extends MainCommand


  override protected def letterCommands = Seq(CreateFromInitial, LoadFromIntermediate, WriteOnceToFinal, Quit)


  trait ParameterizedBySourceSystemAndTableName {
    def sourceSystem(arguments: Seq[String]): String = arguments.head
    def objectName(arguments: Seq[String]): String = arguments(1)
  }

}
