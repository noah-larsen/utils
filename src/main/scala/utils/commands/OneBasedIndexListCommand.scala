package utils.commands

trait OneBasedIndexListCommand extends OneBasedIndexCommand {

  override def usage: String = {
    val nameSymbol = "# [# ...]"
    usage(nameSymbol)
  }

}
