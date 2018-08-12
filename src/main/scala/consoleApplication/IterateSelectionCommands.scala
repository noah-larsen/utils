package consoleApplication

import utils.commands.Parameter.ListParameter
import utils.commands.{Command, Commands, Parameter}
import utils.enumerated.SelfNamed

import scala.util.Try

object IterateSelectionCommands extends Commands {

  override type CommandType = IterateSelectionCommand
  sealed abstract class IterateSelectionCommand(parameters: Seq[Parameter] = Seq()) extends Command(parameters)

  object All extends IterateSelectionCommand
  object Select extends IterateSelectionCommand(Seq(UnfinishedSubrootId))
  object Back extends IterateSelectionCommand


  object UnfinishedSubrootId extends ListParameter(x => Try(x.map(_.toInt)))


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[IterateSelectionCommands.type], classOf[IterateSelectionCommand])

}
