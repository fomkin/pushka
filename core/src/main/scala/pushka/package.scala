package object pushka extends DefaultRWs {

  case class PushkaException(message: String = "") extends Exception("")

  trait RW[T] extends Reader[T] with Writer[T]

  trait Reader[T] {
    def read(value: Ast): T
  }

  trait Writer[T] {
    def write(value: T): Ast
  }

  def read[T](data: Ast)(implicit r: Reader[T]): T = r.read(data)

  def write[T](value: T)(implicit w: Writer[T]): Ast = w.write(value)

}
