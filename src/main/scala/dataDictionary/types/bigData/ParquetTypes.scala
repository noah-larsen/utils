package dataDictionary.types.bigData

import dataDictionary.Type
import dataDictionary.Type.{TypeType, TypesType}
import dataDictionary.types.LogicalFormats
import dataDictionary.types.LogicalFormats.{LogicalFormat, Timestamp}
import utils.enumerated.SelfNamed.NameFormats.{CaseFormats, ObjectName}

object ParquetTypes extends TypesTypeBigData {

  override type T = ParquetType

  sealed abstract class ParquetType extends TypeType(ObjectName(CaseFormats.Lowercase)){

    override protected def withLogicalFormat[T <: ParquetType.this.type](type_ : Type[T]): Option[Type[LogicalFormats.LogicalFormat]] = {
      type_ match {
        case Type(x, None, None) if x == Date => Some(Type(LogicalFormats.Date))
        case Type(x, Some(y), z) if x == Decimal => Some(Type(LogicalFormats.Decimal, Some(y), z))
        case Type(x, None, None) if x == Float => Some(Type(LogicalFormats.Float))
        case Type(x, None, None) if x == Int32 => Some(Type(LogicalFormats.NumericLarge))
        case Type(x, None, None) if x == Int96 => Some(Type(LogicalFormats.NumericBig))
        case Type(x, None, None) if x == String => Some(Type(LogicalFormats.Alphanumeric))
      }
    }

  }

  object Date extends ParquetType
  object Decimal extends ParquetType
  object Float extends ParquetType
  object Int32 extends ParquetType
  object Int96 extends ParquetType
  object String extends ParquetType


  override val values = Seq(Date, Decimal, Float, Int32, Int96, String)


  override def fromLogicalFormat(logicalFormat: Type[LogicalFormat]): Type[ParquetType] = {
    (logicalFormat: @unchecked) match { //todo possible error handling
      case Type(LogicalFormats.Alphanumeric, Some(x), None) => Type(String)
      case Type(LogicalFormats.NumericShort, None, None) => Type(Int32)
      case Type(LogicalFormats.NumericLarge, None, None) => Type(Int32)
      case Type(LogicalFormats.NumericBig, None, None) => Type(Int96)
      case Type(LogicalFormats.Decimal, Some(x), y) => Type(Decimal, Some(x), y)
      case Type(LogicalFormats.Date, None, None) => Type(Date)
      case Type(LogicalFormats.Time, None, None) => Type(String)
      case Type(LogicalFormats.Timestamp, None, None) => Type(String)
      case Type(LogicalFormats.Float, None, None) => Type(Float)
      case Type(LogicalFormats.Double, None, None) => Type(Float)
      case Type(LogicalFormats.Clob, None, None) => Type(String)
      case Type(LogicalFormats.Xml, None, None) => Type(String)
    }
  }

}
