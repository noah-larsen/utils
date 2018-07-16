package consoleApplication

import utils.commands.{Command, Commands, Parameter}

object TableCommands extends Commands {

  override type CommandType = TableCommand
  sealed abstract case class TableCommand(name: String, parameters: Seq[Parameter] = Seq()) extends Command
  object RenameFields extends TableCommand("r")
  object ViewRenamings extends TableCommand("v")
  object Save extends TableCommand("s")
  object WriteOnceToWorkDocument extends TableCommand("w")
  object GoBackWithoutSaving extends TableCommand("b")

  override protected def commands: Seq[CommandType] = Seq(RenameFields, ViewRenamings, Save, WriteOnceToWorkDocument, GoBackWithoutSaving)

}