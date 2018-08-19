package consoleApplication

import utils.commands.Parameter.{ListParameter, OptionalParameter, ValueParameter}
import utils.commands._

import scala.util.Try

object IterateCommands extends Commands {

  override type CommandType = IterateCommand
  sealed abstract class IterateCommand(parameters: Seq[Parameter] = Seq(), specifiedLetterName: Option[Char] = None) extends Command(parameters, specifiedLetterName)

  object Browse extends IterateCommand
  object Search extends IterateCommand(Seq(Keyword))
  object RelatedNodes extends IterateCommand
  object Descendants extends IterateCommand(Seq(MaxDepth))
  object Next extends IterateCommand
  object Finish extends IterateCommand(Seq(FinishedValue1To5))
  object BackToMainMenu extends IterateCommand(specifiedLetterName = Some('m'))


  object FinishedValue1To5 extends ValueParameter(x => Try(x.toInt).filter(finishedValues.contains))
  object MaxDepth extends ValueParameter(x => Try(x.toInt).filter(_ > 0), Some(1))
  object Keyword extends ListParameter


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[IterateCommands.type], classOf[IterateCommand])

}
