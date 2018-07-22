package consoleApplication

import utils.commands.{AbstractCommand, Commands, Command, Parameter}

object MainCommands extends Commands {

  override type CommandType = MainCommand
  sealed abstract class MainCommand(parameters: Seq[Parameter] = Seq()) extends Command(parameters)

  object Load extends MainCommand(Seq(Parameter("sourceSystem"), Parameter("tableName"))) with ParameterizedBySourceSystemAndTableName
  object WriteOnceToDataDictionary extends MainCommand(Seq(Parameter("sourceSystem"), Parameter("tableName"))) with ParameterizedBySourceSystemAndTableName
  object Quit extends MainCommand


  override protected def letterCommands = Seq(Load, WriteOnceToDataDictionary, Quit)


  trait ParameterizedBySourceSystemAndTableName {
    def sourceSystem(arguments: Seq[String]): String = arguments.head
    def dataName(arguments: Seq[String]): String = arguments(1)
  }

}
