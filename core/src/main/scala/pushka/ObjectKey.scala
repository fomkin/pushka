package pushka

trait ObjectKey[T] {
  def stringify(x: T): String
  def parse(x: String): T
}
