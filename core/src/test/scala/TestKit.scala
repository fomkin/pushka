import pushka.{Writer, Reader, Ast}

trait TestKit {
  def write[A](value: A)(implicit w: Writer[A]): Ast = {
    w.write(value)
  }

  def read[A](value: Ast)(implicit r: Reader[A]): A = {
    r.read(value)
  }
}
