package consoleApplication

import utils.commands.Parameter.{ListParameter, OptionalParameter, ValueParameter}
import utils.commands._

import scala.util.Try

object IterateCommands extends Commands {

  override type CommandType = IterateCommand
  sealed abstract class IterateCommand(parameters: Seq[Parameter] = Seq()) extends Command(parameters)

  object MakeNewRelatedNode extends IterateCommand(Seq(NodeAlongPath))
  object RelatedNodes extends IterateCommand
  object Next extends IterateCommand(Seq(FinishedValue1To5))
  object BackToMainMenu extends IterateCommand


  object NodeAlongPath extends ListParameter
  object FinishedValue1To5 extends OptionalParameter(x => Try(x.toInt).filter(finishedValues.contains))


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[IterateCommands.type], classOf[IterateCommand])

}
