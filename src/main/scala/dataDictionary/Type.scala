package dataDictionary

import dataDictionary.Type.{Arguments, TypeType, TypesType}
import dataDictionary.types.DB2Types.DB2Type
import dataDictionary.types.LogicalFormats.LogicalFormat
import utils.enumerated.EnumeratedType.NameFormats.{NameFormat, ObjectName}
import utils.enumerated.{Enumerated, EnumeratedType}

import scala.util.Try

case class Type[T <: TypeType](
                                typeType: T,
                                arg1: Option[Int] = None,
                                arg2: Option[Int] = None
                                ) {

  def logicalFormat: Option[Type[LogicalFormat]] = {
    typeType.logicalFormat(arg1, arg2)
  }


  def string: String = {
    typeType.name + arg1.map(Arguments.prefix + _ + arg2.map(Arguments.separator + _).getOrElse(new String) + Arguments.suffix).getOrElse(new String)
  }

}

object Type {

  def apply(type_ : String, typesType: TypesType): Option[Type[TypeType]] = {
    type_.indexOf(Arguments.prefix) match {
      case -1 => typesType.withName(type_).map(Type[TypeType](_))
      case x =>
        Some(x)
          .map(y => (y, type_.indexOf(Arguments.suffix, y)))
          .filter(y => !y.productIterator.contains(-1) && type_.substring(y._2).tail.trim == new String)
          .map(y => type_.substring(y._1 + 1, y._2).split(Arguments.separator).map(z => Try(z.toInt)))
          .filter(y => y.forall(_.isSuccess) && (0 to 1).contains(y.length - 1))
          .map(_.map(_.get))
          .map(y => (Option(y.head), Some(Unit).filter(_ => y.length > 1).map(_ => y(1))))
          .flatMap(y => typesType.withName(type_.substring(0, x)).map(Type[TypeType](_, y._1, y._2)))
    }
  }


  trait TypesType extends Enumerated {
    override type T <: TypeType
  }


  abstract class TypeType(nameFormat: NameFormat = ObjectName()) extends EnumeratedType(nameFormat){

    def logicalFormat(arg1: Option[Int] = None, arg2: Option[Int] = None): Option[Type[LogicalFormat]] = {
      withLogicalFormat(Type[this.type](this, arg1, arg2))
    }


    protected def withLogicalFormat[T <: this.type](type_ : Type[T]): Option[Type[LogicalFormat]]

  }


  private object Arguments {
    val prefix = "("
    val suffix = ")"
    val separator = ","
  }

}