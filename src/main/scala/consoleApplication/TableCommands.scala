package consoleApplication

import utils.commands.{AbstractCommand, Commands, Command, Parameter}

object TableCommands extends Commands {

  override type CommandType = TableCommand
  sealed abstract class TableCommand(specifiedLetterName: Option[Char] = None) extends Command(specifiedLetterName = specifiedLetterName)

  object RenameFields extends TableCommand
  object ViewRenamings extends TableCommand
  object Save extends TableCommand
  object WriteOnceToWorkDocument extends TableCommand
  object GoBackWithoutSaving extends TableCommand(Some('b'))


  override protected def letterCommands = Seq(RenameFields, ViewRenamings, Save, WriteOnceToWorkDocument, GoBackWithoutSaving)

}