package consoleApplication

import initialDataDictionary.sourceSystem.SourceSystem
import utils.commands.Parameter.ValueParameter
import utils.commands.{Command, Commands, Parameter}

object MainCommands extends Commands {

  override type CommandType = MainCommand
  sealed abstract class MainCommand(parameters: Seq[Parameter] = Seq()) extends Command(parameters)

  object Load extends MainCommand(Seq(SourceSystem, TableName))
  object WriteToFinal extends MainCommand(Seq(SourceSystem, TableName))
  object Quit extends MainCommand


  object SourceSystem extends ValueParameter
  object TableName extends ValueParameter


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[MainCommands.type], classOf[MainCommand])

}
