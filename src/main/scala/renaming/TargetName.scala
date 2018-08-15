package renaming

import centralNamingsRepository.GlobalNamingsRow
import consoleApplication.ConsoleRenamer.Languages
import consoleApplication.ConsoleRenamer.Languages.Language
import dataDictionary.field.FieldEntry

case class TargetName(
                       name: String,
                       languageToLogicalName: Map[Language, String],
                       languageToDescription: Map[Language, String],
                       isApproved: Boolean
                     ) extends Name {

  def logicalName(implicit language: Language): String = {
    languageToLogicalName(language)
  }


  def description(implicit language: Language): String = {
    languageToDescription(language)
  }

}

object TargetName {

  def apply(globalNamingsRow: GlobalNamingsRow): TargetName = {
    val languageToLogicalName = Map(Languages.English -> globalNamingsRow.logicalNameTheFieldEnglish, Languages.Spanish -> globalNamingsRow.logicalNameTheFieldSpanish)
    val languageToDescription = Map(Languages.English -> globalNamingsRow.fieldDescriptionEnglish, Languages.Spanish -> globalNamingsRow.fieldDescriptionSpanish)
    TargetName(globalNamingsRow.globalNamingField, languageToLogicalName, languageToDescription, isApproved = true)
  }


  def apply(fieldEntry: FieldEntry, isApproved: Boolean)(implicit language: Language): TargetName = {
    TargetName(fieldEntry.physicalNameField.getOrElse(new String), Map(language -> fieldEntry.logicalNameField.getOrElse(new String)), Map(language -> fieldEntry.simpleFieldDescription.getOrElse(new String)), isApproved)
  }

}


