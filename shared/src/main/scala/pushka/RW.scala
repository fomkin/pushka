package pushka

trait RW[T] {
  def read(value: Value): T
  def write(value: T): Value
}
