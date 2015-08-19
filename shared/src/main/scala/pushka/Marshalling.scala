package pushka

trait Marshalling {
  def read(value: String): Value
  def write(value: Value): String
}
