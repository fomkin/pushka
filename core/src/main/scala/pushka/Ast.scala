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
} 
