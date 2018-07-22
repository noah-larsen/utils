package utils.commands

abstract class Command(
                        val parameters: Seq[Parameter] = Seq(),
                        specifiedLetterName: Option[Char] = None
                      ) extends AbstractCommand{

  def letterName: Char = {
    specifiedLetterName.getOrElse(name.head.toLower)
  }


  override def usage: String = {
    usage(letterName)
  }

}
