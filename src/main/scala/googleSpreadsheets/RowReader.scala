package googleSpreadsheets

trait RowReader[T <: Row] {

  def sheetRange: SheetRange


  def toRow(row: Seq[String]): T = {
    toRow(x => row.applyOrElse(x, (x: Int) => new String))
  }


  protected def toRow(r: Int => String): T


  protected def withDefaultIfEmpty[U](value: String, transform: String => U, default: U): U = {
    if(value.isEmpty) default else transform(value)
  }


  protected def withDefaultIfEmpty(value: String, default: String): String = {
    Some(value).filter(_.nonEmpty).getOrElse(default)
  }

}

