package googleSpreadsheets

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import utils.enumerated.Enumerated

import scala.util.Try

trait RowReaderWriter[T <: Row] extends RowReader[T] {

  def toStringSeq(row: T): Seq[String] = {
    columns.map(_.string(row))
  }


  protected def columns: Seq[Column[T]]


  protected def date(value: String, possibleFormats: Seq[String]): Try[LocalDate] = {
    possibleFormats.map(x => Try(LocalDate.parse(value, DateTimeFormatter.ofPattern(x)))).maxBy(_.isSuccess)
  }


  protected def dropDown(enumerated: Enumerated, value: String): Option[enumerated.T] = {
    Some(value).filter(_ != new String).flatMap(x => enumerated.withName(x))
  }


  protected def list(value: String, delimiter: String): Seq[String] = {
    value.split(delimiter).toSeq match {
      case x if x == Seq(new String) => Seq()
      case x => x
    }
  }

}

