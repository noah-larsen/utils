package centralNamingsRepository

import googleSpreadsheets.{RowReader, Row, SheetRange}
import renaming.TargetName
import consoleApplication.ConsoleRenamer.Languages
import consoleApplication.ConsoleRenamer.Languages.Language

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
                           ) extends Row {

}

object GlobalNamingsRow extends RowReader[GlobalNamingsRow] {

  override def sheetRange: SheetRange = SheetRange("GLOBAL - Central Repository of Namings", "P", 5)


  override def toRow(r: Int => String): GlobalNamingsRow = {
    GlobalNamingsRow(r(0), r(1), r(2), r(3), r(4), r(5), r(6), r(7), r(8), r(9), r(10), r(11), r(12), r(13), r(14), r(15))
  }

}
