package utils.commands

trait OneBasedIndexCommand extends AbstractCommand {

  override def usage: String = {
    val nameSymbol = "#"
    usage(nameSymbol)
  }

}