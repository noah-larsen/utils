package consoleApplication

import utils.commands.AbstractParameter.{ListParameter, Parameter}
import utils.commands._

import scala.util.Try

object IterateCommands extends Commands {

  override type CommandType = IterateCommand
  sealed abstract class IterateCommand(parameters: Seq[AbstractParameter] = Seq()) extends Command(parameters)

  object MakeNewRelatedNode extends IterateCommand(Seq(NodeAlongPath))
  object RelatedNodes extends IterateCommand
  object Next extends IterateCommand(Seq(FinishedValueForThisNode1To5))
  object BackToMainMenu extends IterateCommand


  object NodeAlongPath extends ListParameter
  object FinishedValueForThisNode1To5 extends Parameter(x => Try(x.toInt).filter(finishedValues.contains), Some(1))


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[IterateCommands.type], classOf[IterateCommand])

}
