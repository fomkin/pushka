package pushka

sealed trait Value

object Value {
  case object Null extends Value
  case object False extends Value
  case object True extends Value
  
  case class Number(value: Double) extends Value
  case class Str(value: String) extends Value
  case class Arr(value: List[Value]) extends Value
  case class Obj(value: Map[String, Value]) extends Value
} 
