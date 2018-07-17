package consoleApplication

import utils.commands.{Command, Commands, Parameter}

object MainCommands extends Commands {

  override type CommandType = MainCommand
  sealed abstract class MainCommand(letterName: Char, parameters: Seq[Parameter] = Seq()) extends Command(Some(letterName), parameters)

  object Load extends MainCommand('l', Seq(Parameter("sourceSystem"), Parameter("tableName"))) with ParameterizedBySourceSystemAndTableName
  object WriteOnceToDataDictionary extends MainCommand('w', Seq(Parameter("sourceSystem"), Parameter("tableName"))) with ParameterizedBySourceSystemAndTableName
  object Quit extends MainCommand('q')


  override protected def commands: Seq[CommandType] = Seq(Load, WriteOnceToDataDictionary, Quit)


  trait ParameterizedBySourceSystemAndTableName {
    def sourceSystem(arguments: Seq[String]): String = arguments.head
    def dataName(arguments: Seq[String]): String = arguments(1)
  }

}
