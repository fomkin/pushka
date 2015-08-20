package object pushka extends DefaultRWs {

  case class PushkaException(message: Option[String] = None)
    extends Exception(message.fold("Unexpected data")(identity))

  trait RW[T] extends Reader[T] with Writer[T]

  trait Reader[T] {
    def read(value: Value): T
  }

  trait Writer[-T] {
    def write(value: T): Value
  }
  
  def write[T](value: T)(implicit w: Writer[T]): Value = w.write(value)
  def read[T](value: Value)(implicit r: Reader[T]): T = r.read(value)
}
