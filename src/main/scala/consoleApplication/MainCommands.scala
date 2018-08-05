package consoleApplication

import utils.commands.AbstractParameter.Parameter
import utils.commands.{AbstractParameter, Command, Commands}

import scala.util.Try

object MainCommands extends Commands {

  override type CommandType = MainCommand
  sealed abstract class MainCommand(parameters: Seq[AbstractParameter] = Seq()) extends Command(parameters)

  object Iterate extends MainCommand(Seq(MaxFinishedValue1To5))
  object Other extends MainCommand
  object Save extends MainCommand
  object Quit extends MainCommand


  object MaxFinishedValue1To5 extends Parameter(x => Try(x.toInt).filter(finishedValues.contains), Some(1))


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[MainCommands.type], classOf[MainCommand])

}
