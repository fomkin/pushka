package object pushka extends DefaultRWs {
  def write[T](value: T)(implicit rw: RW[T]): Value = rw.write(value)
  def read[T](value: Value)(implicit rw: RW[T]): T = rw.read(value)
}
