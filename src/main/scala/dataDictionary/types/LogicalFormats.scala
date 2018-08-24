package dataDictionary.types

import dataDictionary.Type
import dataDictionary.types.LogicalFormats.Alphanumeric
import initialDataDictionary.enumerations.DataSuperTypes.DataSuperType
import utils.enumerated.SelfNamed.NameFormats.ObjectNameWithSpacesBetweenWords

object LogicalFormats extends SuperTypes {

  override type T = LogicalFormat

  sealed abstract class LogicalFormat extends SuperType(ObjectNameWithSpacesBetweenWords()){

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

  
  def isSubsetOf(subset: Type[LogicalFormat], superset: Type[LogicalFormat]): Boolean = {

    def isSubsetOf(subsetPrecisionAndScale: Option[(Int, Int)], supersetPrecisionAndScale: Option[(Int, Int)]): Boolean = {
      (subsetPrecisionAndScale, supersetPrecisionAndScale) match {
        case (Some((w, x)), Some((y, z))) => w <= y && x <= z
        case _ => false
      }
    }


    def knownPrecisionAndScale(tpe: Type[LogicalFormat]): Option[(Int, Int)] = {
      tpe match {
        case Type(Decimal, Some(x), y) => Some(x, y.getOrElse(0))
        case Type(NumericShort, None, None) => Some(4, 0)
        case Type(NumericLarge, None, None) => Some(9, 0)
        case Type(NumericBig, None, None) => Some(Integer.MAX_VALUE, 0)
        case _ => None
      }
    }


    subset.typeType match {
      case Alphanumeric => subset.typeType == superset.typeType && subset.arg1.exists(x => superset.arg1.exists(x <= _))
      case Clob => subset.typeType == superset.typeType
      case Date => Seq(Date, Timestamp).contains(superset.typeType)
      case Decimal => isSubsetOf(knownPrecisionAndScale(subset), knownPrecisionAndScale(superset))
      case Double => Seq(Double, Float).contains(superset.typeType)
      case Float => Seq(Double, Float).contains(superset.typeType)
      case NumericShort => isSubsetOf(knownPrecisionAndScale(subset), knownPrecisionAndScale(superset))
      case NumericLarge => isSubsetOf(knownPrecisionAndScale(subset), knownPrecisionAndScale(superset))
      case NumericBig => isSubsetOf(knownPrecisionAndScale(subset), knownPrecisionAndScale(superset))
      case Time => subset.typeType == superset.typeType
      case Timestamp => subset.typeType == superset.typeType
      case Xml => subset.typeType == superset.typeType
    }

  }
  

  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[LogicalFormats.type], classOf[LogicalFormat])

}
