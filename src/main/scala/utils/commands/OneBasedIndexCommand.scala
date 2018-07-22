package utils.commands

trait OneBasedIndexCommand extends AbstractCommand {

  override def usage: String = {
    val nameSymbol = '#'
    val descriptionSuffix = s" $nameSymbol"
    usage(nameSymbol) + descriptionSuffix
  }

}