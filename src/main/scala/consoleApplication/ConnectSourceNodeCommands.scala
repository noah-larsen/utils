package consoleApplication

import consoleApplication.CommonParameters.{Keyword, MaxDepth}
import utils.commands.Parameter.{ListParameter, OptionalParameter, ValueParameter}
import utils.commands._

import scala.util.Try

object ConnectSourceNodeCommands extends Commands {

  override type CommandType = ConnectSourceNodeCommand
  sealed abstract class ConnectSourceNodeCommand(parameters: Seq[Parameter] = Seq()) extends Command(parameters)

  object RelatedNodes extends ConnectSourceNodeCommand
  object Descendants extends ConnectSourceNodeCommand(Seq(MaxDepth))
  object LookupTargetNodes extends ConnectSourceNodeCommand
  object SearchTargetNodes extends ConnectSourceNodeCommand(Seq(Keyword))
  object Next extends ConnectSourceNodeCommand
  object Finish extends ConnectSourceNodeCommand(Seq(FinishedValue1To5))
  object BackToMainMenu extends ConnectSourceNodeCommand


  object FinishedValue1To5 extends ValueParameter(x => Try(x.toInt).filter(finishedValues.contains))


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[ConnectSourceNodeCommands.type], classOf[ConnectSourceNodeCommand])

}
