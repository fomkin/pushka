package pushka

sealed trait Value

object Value {
  case object Null extends Value
  case class Number(value: Double) extends Value
  case class Str(value: String) extends Value
  case class Arr(value: Seq[Value]) extends Value
  case class Obj(value: Map[String, Value]) extends Value
} 
