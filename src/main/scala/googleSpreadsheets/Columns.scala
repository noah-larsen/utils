package googleSpreadsheets

import utils.enumerated.{Enumerated, SelfNamed}

trait Columns extends Enumerated {

  type RowType <: Row
  type ColumnType <: Column[RowType]
  override type T = ColumnType


  protected def anyValOptionToString(x: Option[AnyVal]): String = {
    x.map(_.toString).getOrElse(new String)
  }
  protected def selfNamedOptionToString(x: Option[SelfNamed]): String = {
    x.map(_.name).getOrElse(new String)
  }

}
