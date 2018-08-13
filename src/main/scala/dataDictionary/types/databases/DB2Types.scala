package dataDictionary.types.databases

import dataDictionary.Type
import dataDictionary.types.{LogicalFormats, SuperType, SuperTypes}
import dataDictionary.types.LogicalFormats._

object DB2Types extends SuperTypes {

  override type T = DB2Type

  sealed trait DB2Type extends SuperType {

    override protected def withLogicalFormat[T <: this.type](type_ : Type[T]): Option[Type[LogicalFormat]] = {
      type_ match {
        case Type(x, None, None) if x == BigInt => Some(Type(NumericBig))
        case Type(x, Some(y), None) if x == Char => Some(Type(Alphanumeric, Some(y)))
        case Type(x, None, None) if x == Date => Some(Type(LogicalFormats.Date))
        case Type(x, Some(y), None) if x == Decimal && Set(4, 6).contains(y) => Some(Type(NumericShort))
        case Type(x, Some(y), None) if x == Decimal && Set(9, 11).contains(y) => Some(Type(NumericLarge))
        case Type(x, Some(y), None) if x == Decimal => Some(Type(LogicalFormats.Decimal, Some(y)))
        case Type(x, Some(y), Some(z)) if x == Decimal => Some(Type(LogicalFormats.Decimal, Some(y), Some(z)))
        case Type(x, Some(y), None) if x == Float => Some(Type(LogicalFormats.Float))
        case Type(x, None, None) if x == Integer => Some(Type(NumericLarge))
        case Type(x, None, None) if x == SmallInt => Some(Type(NumericShort))
        case Type(x, None, None) if x == Time => Some(Type(Alphanumeric, Some(maxLengthStringTimeValue)))
        case Type(x, None, None) if x == Timestamp => Some(Type(LogicalFormats.Timestamp))
        case _ => None
      }
    }

  }

  object BigInt extends DB2Type
  object Char extends DB2Type
  object Date extends DB2Type
  object Decimal extends DB2Type
  object Float extends DB2Type
  object Integer extends DB2Type
  object SmallInt extends DB2Type
  object Time extends DB2Type
  object Timestamp extends DB2Type


  override val values = Seq(BigInt, Char, Date, Decimal, Float, Integer, SmallInt, Time, Timestamp)


  private val maxLengthStringTimeValue = 100

}
