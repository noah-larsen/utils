package googleSpreadsheets

import googleSpreadsheets.DataReaderWriter.{AbstractColumn, AbstractColumns}
import utils.Enumerated
import utils.Enumerated.EnumeratedType

trait DataReaderWriter[T <: Row] extends DataReader[T] {

  def toStringSeq(row: T): Seq[String] = {
    columns.order.map(_.string(row))
  }


  protected def columns: AbstractColumns[T]


  protected def dropDown(enumerated: Enumerated, value: String): Option[enumerated.T] = {
    Some(value).filter(_ != new String).flatMap(x => enumerated.withName(x))
  }

}

object DataReaderWriter {

  trait AbstractColumns[U] {

    type Column <: AbstractColumn[U]


    def order: Seq[Column]


    protected def enumeratedTypeOptionToString(x: Option[EnumeratedType]): String = x.map(_.name).getOrElse(new String)

  }


  trait AbstractColumn[V] {

    def string: V => String

  }

}
