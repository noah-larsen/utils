package renaming.consoleApplication

import utils.{AbstractCommand, Commands}

object MainCommands extends Commands {

  override type Command = MainCommand
  sealed abstract class MainCommand(val name: String, val parameters: Seq[String] = Seq()) extends AbstractCommand

  case object Load extends MainCommand("l", Seq("sourceSystem", "tableName")) with ParameterizedBySourceSystemAndTableName
  case object WriteOnceToDataDictionary extends MainCommand("w", Seq("sourceSystem", "tableName")) with ParameterizedBySourceSystemAndTableName
  case object Quit extends MainCommand("q")


  override protected def commands: Seq[Command] = Seq(Load, WriteOnceToDataDictionary, Quit)


  trait ParameterizedBySourceSystemAndTableName {
    def sourceSystem(arguments: Seq[String]): String = arguments.head
    def dataName(arguments: Seq[String]): String = arguments(1)
  }

}
