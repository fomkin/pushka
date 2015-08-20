package object pushka extends DefaultRWs {

  case class PushkaException(message: String = "") extends Exception("")

  trait RW[T] extends Reader[T] with Writer[T]

  trait Reader[T] {
    def read(value: Ast): T
  }

  trait Writer[-T] {
    def write(value: T): Ast
  }

}
