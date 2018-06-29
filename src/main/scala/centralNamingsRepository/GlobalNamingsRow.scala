package centralNamingsRepository

import googleSpreadsheets.{DataReader, Row, SheetRange}
import renaming.ApprovedName
import renaming.consoleApplication.ConsoleRenamer.Languages

case class GlobalNamingsRow(
                                                               termCode: String,
                                                               fieldCode: String,
                                                               globalNamingField: String,
                                                               globalFormatDomainPlusLength: String,
                                                               hierarchy: String,
                                                               logicalNameCode: String,
                                                               logicalNameTheFieldSpanish: String,
                                                               fieldDescriptionSpanish: String,
                                                               logicalNameTheFieldEnglish: String,
                                                               fieldDescriptionEnglish: String,
                                                               functionalGroupingPhysicalFieldsLevel1: String,
                                                               functionalGroupingPhysicalFieldsLevel2: String,
                                                               functionalGroupingPhysicalFieldsLevel3: String,
                                                               tagsPhysicalFields: String,
                                                               relatedFields: String,
                                                               dateLastModification: String
                                                               //todo
                                                             ) extends Row with ApprovedName{

  override def name: String = {
    globalNamingField
  }


  override def logicalName(implicit language: Languages.Value): String = {
    language match {
      case Languages.english => logicalNameTheFieldEnglish
      case Languages.spanish => logicalNameTheFieldSpanish
    }
  }


  override def description(implicit language: Languages.Value): String = {
    language match {
      case Languages.english => fieldDescriptionEnglish
      case Languages.spanish => fieldDescriptionSpanish
    }
  }

}

object GlobalNamingsRow extends DataReader[GlobalNamingsRow] {

  override def sheetRange: SheetRange = SheetRange("GLOBAL - Central Repository of Namings", "P", 5)


  override def toRow(r: Int => String): GlobalNamingsRow = {
    GlobalNamingsRow(r(0), r(1), r(2), r(3), r(4), r(5), r(6), r(7), r(8), r(9), r(10), r(11), r(12), r(13), r(14), r(15))
  }

}
