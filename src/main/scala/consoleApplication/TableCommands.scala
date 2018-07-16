package consoleApplication

import utils.commands.{Command, Commands, Parameter}

object TableCommands extends Commands {

  override type CommandType = TableCommand
  sealed abstract class TableCommand(name: String) extends Command(name)
  object RenameFields extends TableCommand("r")
  object ViewRenamings extends TableCommand("v")
  object Save extends TableCommand("s")
  object WriteOnceToWorkDocument extends TableCommand("w")
  object GoBackWithoutSaving extends TableCommand("b")

  override protected def commands: Seq[CommandType] = Seq(RenameFields, ViewRenamings, Save, WriteOnceToWorkDocument, GoBackWithoutSaving)

}