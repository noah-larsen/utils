package dataDictionary

import dataDictionary.Type.Arguments
import dataDictionary.types.LogicalFormats.{Alphanumeric, LogicalFormat}
import dataDictionary.types.{LogicalFormats, SuperType, SuperTypes}
import initialDataDictionary.enumerations.DataSuperTypes.DataSuperType

import scala.util.Try

case class Type[T <: SuperType](
                                typeType: T,
                                arg1: Option[Int] = None,
                                arg2: Option[Int] = None
                                ) {

  def isLogicalSubsetOf(argType: Type[_]): Boolean = {
    (logicalFormat, argType.logicalFormat) match {
      case (Some(thisTypeLogicalFormat), Some(thatTypeLogicalFormat)) =>
        LogicalFormats.isSubsetOf(thisTypeLogicalFormat, thatTypeLogicalFormat)
      case _ => false
    }
  }


  def logicalFormat: Option[Type[LogicalFormat]] = {
    typeType.logicalFormat(arg1, arg2)
  }


  def asString: String = {
    typeType.name + arg1.map(Arguments.prefix + _ + arg2.map(Arguments.separator + _).getOrElse(new String) + Arguments.suffix).getOrElse(new String)
  }

}

object Type {

  def apply(tpe: String, typeTypes: SuperTypes): Option[Type[SuperType]] = {
    tpe.indexOf(Arguments.prefix) match {
      case -1 => typeTypes.withName(tpe).map(Type[SuperType](_))
      case x =>
        Some(x)
          .map(y => (y, tpe.indexOf(Arguments.suffix, y)))
          .filter(y => !y.productIterator.contains(-1) && tpe.substring(y._2).tail.trim == new String)
          .map(y => tpe.substring(y._1 + 1, y._2).split(Arguments.separator).map(z => Try(z.toInt)))
          .filter(y => y.forall(_.isSuccess) && (0 to 1).contains(y.length - 1))
          .map(_.map(_.get))
          .map(y => (Option(y.head), Some(Unit).filter(_ => y.length > 1).map(_ => y(1))))
          .flatMap(y => typeTypes.withName(tpe.substring(0, x)).map(Type[SuperType](_, y._1, y._2)))
    }
  }


  def logicalFormat(logicalFormat: String): Option[Type[LogicalFormat]] = {
    apply(logicalFormat, LogicalFormats).flatMap(_.logicalFormat)
  }


  def logicalFormat(dataType: String, dataSuperType: DataSuperType): Option[Type[LogicalFormat]] = {
    Type(dataType, SuperTypes.from(dataSuperType)).flatMap(_.logicalFormat)
  }


  private object Arguments {
    val prefix = "("
    val suffix = ")"
    val separator = ","
  }

}