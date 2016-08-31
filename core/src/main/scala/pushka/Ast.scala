package pushka

sealed trait Ast

object Ast {
  case object Null extends Ast
  case object False extends Ast
  case object True extends Ast
  
  case class Num(value: String) extends Ast
  object Num extends (String => Num) {
    def apply(value: Double): Num = Num(value.toString)
    def apply(value: Int): Num = Num(value.toString)
    def apply(value: Float): Num = Num(value.toString)
    def apply(value: Byte): Num = Num(value.toString)
    def apply(value: Short): Num = Num(value.toString)
    def apply(value: Long): Num = Num(value.toString)
  }

  case class Str(value: String) extends Ast
  case class Arr(value: Iterable[Ast]) extends Ast
  case class Obj(value: Map[String, Ast]) extends Ast

  private[this] def convert(value: Any): Ast = value match {
    case x: Ast ⇒ x
    case x: Byte ⇒ Num(x.toString)
    case x: Short ⇒ Num(x.toString)
    case x: Int ⇒ Num(x.toString)
    case x: Float ⇒ Num(x.toString)
    case x: Double ⇒ Num(x.toString)
    case x: Long ⇒ Num(x.toString)
    case x: Boolean if x ⇒ True
    case x: Boolean if !x ⇒ False
    case x: String ⇒ Str(x)
    case x: Seq[_] ⇒ Arr(x.map(convert).toList)
  }

  def apply(pairs: (String, Any)*): Obj = {
    val astPairs = pairs map { case (key, value) ⇒ (key, convert(value)) }
    Obj(astPairs.toMap)
  }
} 
