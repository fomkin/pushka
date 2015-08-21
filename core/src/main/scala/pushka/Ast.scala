package pushka

sealed trait Ast

object Ast {
  case object Null extends Ast
  case object False extends Ast
  case object True extends Ast
  
  case class Num(value: Double) extends Ast
  case class Str(value: String) extends Ast
  case class Arr(value: List[Ast]) extends Ast
  case class Obj(value: Map[String, Ast]) extends Ast

  private[this] def convert(value: Any): Ast = value match {
    case x: Ast ⇒ x
    case x: Byte ⇒ Num(x)
    case x: Short ⇒ Num(x)
    case x: Int ⇒ Num(x)
    case x: Float ⇒ Num(x)
    case x: Double ⇒ Num(x)
    case x: Long ⇒ Num(x)
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
