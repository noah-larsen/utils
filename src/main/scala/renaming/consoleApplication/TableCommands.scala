package renaming.consoleApplication

import utils.{AbstractCommand, Commands}

object TableCommands extends Commands {

  override type Command = TableCommand
  sealed abstract class TableCommand(val name: String, val parameters: Seq[String] = Seq()) extends AbstractCommand
  case object IterateUnnamedFields extends TableCommand("i")
  case object ViewRenamings extends TableCommand("v")
  case object Save extends TableCommand("s")
  case object WriteOnceToWorkDocumentAndSave extends TableCommand("w")
  case object GoBackWithoutSaving extends TableCommand("b")

  override protected def commands: Seq[Command] = Seq(IterateUnnamedFields, ViewRenamings, Save, WriteOnceToWorkDocumentAndSave, GoBackWithoutSaving)

}