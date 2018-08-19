package consoleApplication

import utils.commands.{Command, Commands, OneBasedIndexCommand, Parameter}

object BrowseCommands extends Commands {

  override type CommandType = BrowseCommand
  sealed abstract class BrowseCommand(parameters: Seq[Parameter] = Seq(), specifiedLetterName: Option[Char] = None) extends Command(parameters, specifiedLetterName)

  object GoTo extends BrowseCommand with OneBasedIndexCommand
  object GoUp extends BrowseCommand(specifiedLetterName = Some('u'))
  object MarkRelated extends BrowseCommand
  object CreateNewNode extends BrowseCommand
  object Back extends BrowseCommand


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[BrowseCommands.type], classOf[BrowseCommand])

}
