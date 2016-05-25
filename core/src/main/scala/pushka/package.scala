package object pushka extends DefaultRWs with DefaultObjectKeys {

  object PushkaException {

    def apply(value: Ast): PushkaException = {
      new PushkaException(s"Error while reading AST $value")
    }

    def apply(value: Ast, typeName: String): PushkaException = {
      new PushkaException(s"Error while reading AST $value to $typeName")
    }

    def apply(value: Ast, klass: Class[_]): PushkaException = {
      new PushkaException(s"Error while reading AST $value to ${klass.getSimpleName.split("\\$").last}")
    }

    def apply[T](value: T): PushkaException = {
      new PushkaException(s"Error while writing value $value")
    }
  }

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
