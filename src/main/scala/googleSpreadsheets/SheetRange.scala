package googleSpreadsheets

case class SheetRange(sheetName: String, toColumn: String, fromRow: Int = 1, fromColumn: String = "A", toRow: Option[Int] = None) {

  def range: String = {
    s"$sheetName!$fromColumn$fromRow:$toColumn${toRow.getOrElse(new String)}"
  }

}
