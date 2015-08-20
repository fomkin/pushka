package pushka

/**
 * Print AST to a data of T
 * @tparam T type of data
 */
trait Printer[T] {
  def print(value: Ast): T
}
