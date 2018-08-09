package googleSpreadsheets

import utils.enumerated.{Enumerated, SelfNamed}

trait Columns extends Enumerated {

  type RowType <: Row
  type ColumnType <: Column[RowType]
  override type T = ColumnType


  protected def selfNamedOptionToString(x: Option[SelfNamed]): String = x.map(_.name).getOrElse(new String)

}
