package consoleApplication

import utils.commands.Parameter.ValueParameter
import utils.commands.{Parameter, Command, Commands}
import utils.enumerated.Enumerated

object OtherCommands extends Commands {

  override type CommandType = OtherCommand
  sealed abstract class OtherCommand(parameters: Seq[Parameter] = Seq()) extends Command(parameters)

  object InitializeGoogleProductTaxonomy extends OtherCommand(Seq(Pathname, ForestLabel))
  object Back extends OtherCommand


  object Pathname extends ValueParameter
  object ForestLabel extends ValueParameter


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[OtherCommands.type], classOf[OtherCommand])

}
