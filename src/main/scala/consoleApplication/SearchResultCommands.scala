package consoleApplication

import utils.commands.{Command, Commands, OneBasedIndexCommand, OneBasedIndexListCommand}

object SearchResultCommands extends Commands {

  override type CommandType = SearchResultCommand
  sealed abstract class SearchResultCommand extends Command

  object GoToResultNumber extends SearchResultCommand with OneBasedIndexCommand
  object Back extends SearchResultCommand

  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[SearchResultCommands.type], classOf[SearchResultCommand])

}
