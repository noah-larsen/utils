package consoleApplication

import consoleApplication.MainCommands.ParameterizedBySourceSystemAndTableName
import utils.commands.{AbstractCommand, Command, Commands, Parameter}

object MainCommands extends Commands {

  override type CommandType = MainCommand
  sealed abstract class MainCommand(parameters: Seq[Parameter] = Seq()) extends Command(parameters)

  object CreateFromInitialDataDictionary extends MainCommand(Seq(Parameter("sourceSystem"), Parameter("tableName"))) with ParameterizedBySourceSystemAndTableName
  object LoadFromIntermediateDataDictionary extends MainCommand(Seq(Parameter("sourceSystem"), Parameter("tableName"))) with ParameterizedBySourceSystemAndTableName
  object WriteOnceToDataDictionary extends MainCommand(Seq(Parameter("sourceSystem"), Parameter("tableName"))) with ParameterizedBySourceSystemAndTableName
  object Quit extends MainCommand


  override protected def letterCommands = Seq(LoadFromIntermediateDataDictionary, WriteOnceToDataDictionary, Quit)


  trait ParameterizedBySourceSystemAndTableName {
    def sourceSystem(arguments: Seq[String]): String = arguments.head
    def objectName(arguments: Seq[String]): String = arguments(1)
  }

}
