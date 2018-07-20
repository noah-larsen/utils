package dataDictionary.types.databases

import dataDictionary.Type
import dataDictionary.Type.{TypeType, TypesType}
import dataDictionary.types.LogicalFormats
import dataDictionary.types.LogicalFormats._

object OracleTypes extends TypesType {

  override type T = OracleType

  sealed trait OracleType extends TypeType {

    override protected def withLogicalFormat[T <: this.type](type_ : Type[T]): Option[Type[LogicalFormat]] = {
      type_ match {
        case Type(x, None, None) if x == Date => None //todo
        case Type(x, Some(y), None) if x == Float => Some(Type(LogicalFormats.Float, Some(y)))
        case Type(x, Some(y), None) if x == Number && Set(4, 6).contains(y) => Some(Type(NumericShort, Some(y)))
        case Type(x, Some(y), None) if x == Number && Set(9, 11).contains(y) => Some(Type(NumericLarge, Some(y)))
        case Type(x, Some(y), None) if x == Number && Set(19).contains(y) => Some(Type(NumericBig, Some(y)))
        case Type(x, Some(y), z) if x == Number => Some(Type(Decimal, Some(y), z))
        case Type(x, None, None) if x == Timestamp => Some(Type(LogicalFormats.Timestamp))
        case Type(x, Some(y), None) if x == Varchar2 => Some(Type(Alphanumeric, Some(y)))
        case _ => None
      }
    }

  }

  object Date extends OracleType
  object Float extends OracleType
  object Number extends OracleType
  object Timestamp extends OracleType
  object Varchar2 extends OracleType


  override val values = Seq(Date, Float, Number, Timestamp, Varchar2)

}
