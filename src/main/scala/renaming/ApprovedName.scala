package renaming

import centralNamingsRepository.GlobalNamingsRow
import consoleApplication.ConsoleRenamer.Languages
import consoleApplication.ConsoleRenamer.Languages.Language

case class ApprovedName(
                         name: String,
                         languageToLogicalName: Map[Language, String],
                         languageToDescription: Map[Language, String]
                       ) extends Name {

  def logicalName(implicit language: Language): String = {
    languageToLogicalName(language)
  }


  def description(implicit language: Language): String = {
    languageToDescription(language)
  }

}

object ApprovedName {

  def apply(globalNamingsRow: GlobalNamingsRow): ApprovedName = {
    ApprovedName(
      globalNamingsRow.globalNamingField,
      Map(
        Languages.English -> globalNamingsRow.logicalNameTheFieldEnglish,
        Languages.Spanish -> globalNamingsRow.logicalNameTheFieldSpanish
      ),
      Map(
        Languages.English -> globalNamingsRow.fieldDescriptionEnglish,
        Languages.Spanish -> globalNamingsRow.fieldDescriptionSpanish
      )
    )
  }

}


