package dataDictionary.types

import dataDictionary.Type
import dataDictionary.types.LogicalFormats.LogicalFormat
import utils.enumerated.SelfNamed
import utils.enumerated.SelfNamed.NameFormats.{NameFormat, ObjectName}

abstract class SuperType(nameFormat: NameFormat = ObjectName()) extends SelfNamed(nameFormat){

  def logicalFormat(arg1: Option[Int] = None, arg2: Option[Int] = None): Option[Type[LogicalFormat]] = {
    withLogicalFormat(Type[this.type](this, arg1, arg2))
  }


  protected def withLogicalFormat[T <: this.type](type_ : Type[T]): Option[Type[LogicalFormat]]

}