package utils.enumerated

import scala.reflect.api.JavaUniverse
import scala.util.Try

/**
  * An enumeration trait that combines the more straightforward typing and access to sealed types of the typed-objects within objects
  * approach with the access to self-referentially named strings and collections of the enumerated values of the Enumeration class. The
  * enumerated values are instances of the type member, and must be objects declared directly inside an Enumerated object. The order of the
  * object declarations determines the order within the output of the values method. The trait does require a small amount of boilerplate.
  * After defining the type member, set enumeratedTypes equal to EnumeratedTypes(u.typeOf[ENUMERATED_OBJECT.type], classOf[TYPE_MEMBER])
  * with the corresponding values for ENUMERATED_OBJECT and TYPE_MEMBER.
  */
trait Enumerated {

  type T <: SelfNamed


  def values: Seq[T] = {
    enumeratedTypes.typeSelf.decls.filter(_.isModule).map(x => Try(u.runtimeMirror(this.getClass.getClassLoader).reflectModule(x.asModule))
      .recover{case e: ScalaReflectionException => u.runtimeMirror(this.getClass.getClassLoader).reflect(this).reflectModule(x.asModule)}
      .get.instance).collect{case x if enumeratedTypes.classT.isInstance(x) => x.asInstanceOf[T]}.toSeq
  }


  def withName(name: String, ignoreCase: Boolean = true, normalizeSpaceChars: Boolean = true, trim: Boolean = true): Option[T] = {

    def process(value: String) = {
      val normalSpace = " "
      Some(value).map(x => if(ignoreCase) x.toLowerCase else x).map(x => if(normalizeSpaceChars) x.map(y => if(y.isSpaceChar)
        normalSpace else y).mkString else x).map(x => if(trim) x.trim else x).get
    }


    values.find(x => process(x.name) == process(name))

  }


  protected val enumeratedTypes: EnumeratedTypes


  protected val u: JavaUniverse = scala.reflect.runtime.universe
  protected case class EnumeratedTypes(typeSelf: u.Type, classT: Class[T])

}