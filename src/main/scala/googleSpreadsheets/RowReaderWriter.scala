package googleSpreadsheets

import googleSpreadsheets.RowReaderWriter.{AbstractColumn, AbstractColumns}
import utils.enumerated.Enumerated
import utils.enumerated.SelfNamed

trait RowReaderWriter[T <: Row] extends RowReader[T] {

  def toStringSeq(row: T): Seq[String] = {
    columns.order.map(_.string(row))
  }


  protected def columns: AbstractColumns[T]


  protected def dropDown(enumerated: Enumerated, value: String): Option[enumerated.T] = {
    Some(value).filter(_ != new String).flatMap(x => enumerated.withName(x))
  }

}

object RowReaderWriter {

  trait AbstractColumns[U] {

    type Column <: AbstractColumn[U]


    def order: Seq[Column]


    protected def enumeratedTypeOptionToString(x: Option[SelfNamed]): String = x.map(_.name).getOrElse(new String)

  }


  trait AbstractColumn[V] {

    def string: V => String

  }

}
