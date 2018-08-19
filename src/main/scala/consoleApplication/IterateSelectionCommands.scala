package consoleApplication

import utils.commands.Parameter.ListParameter
import utils.commands.{Command, Commands, OneBasedIndexListCommand, Parameter}
import utils.enumerated.SelfNamed

import scala.util.Try

object IterateSelectionCommands extends Commands {

  override type CommandType = IterateSelectionCommand
  sealed abstract class IterateSelectionCommand(parameters: Seq[Parameter] = Seq(), specifiedLetterName: Option[Char] = None) extends Command(parameters, specifiedLetterName)

  object SelectNodes extends IterateSelectionCommand with OneBasedIndexListCommand
  object SelectAll extends IterateSelectionCommand(specifiedLetterName = Some('a'))
  object Back extends IterateSelectionCommand


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[IterateSelectionCommands.type], classOf[IterateSelectionCommand])

}
