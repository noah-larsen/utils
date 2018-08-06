package googleSpreadsheets

trait RowReader[T <: Row] {

  def sheetRange: SheetRange


  def toRow(row: Seq[String]): T = {
    toRow(x => row.applyOrElse(x, (x: Int) => new String))
  }


  protected def toRow(r: Int => String): T

}

