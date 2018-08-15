package object googleSpreadsheets {

  val googleSpreadsheetModifiedDateFormat = "d/M/yyyy"


  def asPlainTextSpreadsheetFormula(value: String): String = {
    "\"" match {case dq => s"=t($dq$value$dq)"}
  }

}
