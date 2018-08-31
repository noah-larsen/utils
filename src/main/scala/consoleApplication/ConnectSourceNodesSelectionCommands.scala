package consoleApplication

import utils.commands.IndexedCommand.IndexListCommand
import utils.commands.Parameter.ListParameter
import utils.commands.{Command, Commands, Parameter}
import utils.enumerated.SelfNamed

import scala.util.Try

object ConnectSourceNodesSelectionCommands extends Commands {

  override type CommandType = ConnectSourceNodesSelectionCommand
  sealed abstract class ConnectSourceNodesSelectionCommand(parameters: Seq[Parameter] = Seq(), specifiedLetterName: Option[Char] = None) extends Command(parameters, specifiedLetterName)

  object SelectNodes extends ConnectSourceNodesSelectionCommand with IndexListCommand
  object SelectAll extends ConnectSourceNodesSelectionCommand(specifiedLetterName = Some('a'))
  object Back extends ConnectSourceNodesSelectionCommand


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[ConnectSourceNodesSelectionCommands.type], classOf[ConnectSourceNodesSelectionCommand])

}
