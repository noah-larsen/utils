package dataDictionary.types

import dataDictionary.Type
import dataDictionary.Type.{TypeType, TypesType}
import utils.enumerated.SelfNamed.NameFormats.ObjectNameWithSpacesBetweenWords

object LogicalFormats extends TypesType {

  override type T = LogicalFormat

  sealed abstract class LogicalFormat extends TypeType(ObjectNameWithSpacesBetweenWords()){

    override def withLogicalFormat[T <: this.type](type_ : Type[T]): Option[Type[LogicalFormat]] = {
      Some(type_.asInstanceOf[Type[LogicalFormat]])
    }

  }

  object Alphanumeric extends LogicalFormat
  object Clob extends LogicalFormat
  object Date extends LogicalFormat
  object Decimal extends LogicalFormat
  object Double extends LogicalFormat
  object Float extends LogicalFormat
  object NumericShort extends LogicalFormat
  object NumericLarge extends LogicalFormat
  object NumericBig extends LogicalFormat
  object Time extends LogicalFormat
  object Timestamp extends LogicalFormat
  object Xml extends LogicalFormat


  override val values = Seq(Alphanumeric, Clob, Date, Decimal, Double, Float, NumericShort, NumericLarge, NumericBig, Time, Timestamp, Xml)

}
