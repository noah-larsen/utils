package dataDictionary.types

import dataDictionary.Type
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


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[LogicalFormats.type], classOf[LogicalFormat])

}
