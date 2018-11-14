package utils.other

case class Lazy[T](private val f: () => T) {

  lazy val get = f()

}
