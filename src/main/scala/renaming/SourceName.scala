package renaming

import consoleApplication.ConsoleRenamer.Languages.Language
import dataDictionary.FieldEntry

case class SourceName(
                  name: String,
                  logicalName: String,
                  description: String,
                  language: Language
                ) extends Name {

}

object SourceName {

  def apply(fieldEntry: FieldEntry)(implicit language: Language): SourceName = {
    SourceName(fieldEntry.sourceField.getOrElse(new String), fieldEntry.logicalNameField.getOrElse(new String), fieldEntry.simpleFieldDescription.getOrElse(new String), language)
  }

}