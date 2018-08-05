package consoleApplication

import utils.commands.AbstractParameter.Parameter
import utils.commands.{AbstractParameter, Command, Commands}
import utils.enumerated.Enumerated

object OtherCommands extends Commands {

  override type CommandType = OtherCommand
  sealed abstract class OtherCommand(parameters: Seq[AbstractParameter] = Seq()) extends Command(parameters)

  object InitializeGoogleProductTaxonomy extends OtherCommand(Seq(Pathname, ForestLabel))
  object Back extends OtherCommand


  object Pathname extends Parameter
  object ForestLabel extends Parameter


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[OtherCommands.type], classOf[OtherCommand])

}
