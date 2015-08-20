package pushka

trait Parser[T] {
  def parse(value: T): Ast
}
