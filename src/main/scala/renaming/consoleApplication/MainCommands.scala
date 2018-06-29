package renaming.consoleApplication

import utils.{AbstractCommand, Commands}

object MainCommands extends Commands {

  override type Command = MainCommand
  sealed abstract class MainCommand(val name: String, val parameters: Seq[String] = Seq()) extends AbstractCommand
  case object Load extends MainCommand("l", Seq("sourceSystem", "tableName")) {
    def sourceSystem(arguments: Seq[String]): String = arguments.head
    def tableName(arguments: Seq[String]): String = arguments(1)
  }
  case object WriteUnwrittenApprovedObjectsToDataDictionaries extends MainCommand("w")
  case object Quit extends MainCommand("q")

  override protected def commands: Seq[Command] = Seq(Load, WriteUnwrittenApprovedObjectsToDataDictionaries, Quit)

}
