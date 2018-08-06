package us.alnova

import googleSpreadsheets.{RowReader, Row, SheetRange}

private[alnova] case class ATLRow(
                           tableName: String,
                           fieldName: String,
                           columnNumber: String,
                           type_ : String,
                           length: String,
                           nullsAllowed: String,
                           description: String
                         ) extends Row{

  def areNullsAllowed: Boolean = {
    //todo error handling, although this is all temporary
    val nullsAllowedString = "Y"
    val nullsNotAllowedString = "N"
    nullsAllowed match {
      case `nullsAllowedString` => true
      case `nullsNotAllowedString` => false
    }
  }

}

private[alnova] object ATLRow extends RowReader[ATLRow] {

  override def sheetRange: SheetRange = SheetRange("ben2", "G", 2)


  override def toRow(row: Int => String): ATLRow = {
    ATLRow(row(0), row(1), row(2), row(3), row(4), row(5), row(6))
  }

}
