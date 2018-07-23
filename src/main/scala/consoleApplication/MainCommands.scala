package consoleApplication

import utils.commands.{Command, Commands}

object MainCommands extends Commands {

  override type CommandType = MainCommand
  sealed abstract class MainCommand extends Command

  object AddTaxonomy extends MainCommand
  object Quit extends MainCommand


  override protected def letterCommands = Seq(AddTaxonomy, Quit)

}
