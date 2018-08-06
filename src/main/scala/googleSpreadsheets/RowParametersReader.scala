package googleSpreadsheets

import googleSpreadsheets.RowParametersReader.{RowParameter, RowParameters}
import utils.enumerated.SelfNamed.NameFormats.{NameFormat, ObjectName}
import utils.enumerated.{Enumerated, SelfNamed}

trait RowParametersReader[T] {

  def sheetRange: SheetRange


  def transform(rows: Seq[Seq[String]]): T = {
    val parameterToValues = rows.filter(_.nonEmpty).map(x => (x.head, x.tail)).toMap.map(x => (rowParameters.withName(x._1), x._2)).collect{case (Some(x), y) => (x, y)}
    transform(parameterToValues.collect{case (x, y) if !x.isList => (x, y.headOption.getOrElse(new String))}, parameterToValues.collect{case x if x._1.isList => x})
  }


  protected def rowParameters: RowParameters
  protected def transform(parameterToValue: Map[RowParameter, String], parameterToValues: Map[RowParameter, Seq[String]]): T


  protected val maxColumn = "ZZ"


  protected def toBooleanOption(parameterToValue: Map[RowParameter, String], rowParameter: RowParameter): Option[Boolean] = {
    Some(parameterToValue(rowParameter)).filter(_.nonEmpty).map(_.toBoolean)
  }

}

object RowParametersReader {

  trait RowParameters extends Enumerated {

    type RowParameterType <: RowParameter
    override type T = RowParameterType

  }


  abstract class RowParameter(
                               val isList: Boolean = false,
                               nameFormat: NameFormat = ObjectName()
                             ) extends SelfNamed(nameFormat)

}
