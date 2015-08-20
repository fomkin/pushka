package pushka.json

import pushka.{Printer, Ast}

import scala.annotation.tailrec

final class JsonPrinter extends Printer[String] {

  @tailrec
  private[this] def arrayToJSONRec(tail: List[Ast], acc: String = ""): String = (acc, tail) match {
    case (_, Nil) ⇒ acc
    case ("", x :: Nil) ⇒ toJSONInternal(x)
    case ("", x :: xs) ⇒ arrayToJSONRec(xs, toJSONInternal(x))
    case (_, x :: Nil) ⇒ acc + "," + toJSONInternal(x)
    case (_, x :: xs) ⇒ arrayToJSONRec(xs, acc + "," + toJSONInternal(x))
  }

  @tailrec
  private[this] def objToJSONRec(tail: List[(String, Ast)], acc: String = ""): String = {
    @inline def w(key: String, value: String) = "\"" + key + "\":" + value
    (acc, tail) match {
      case (_, Nil) ⇒ acc
      case ("", (key, value) :: Nil) ⇒ w(key, toJSONInternal(value))
      case ("", (key, value) :: xs) ⇒ objToJSONRec(xs, w(key, toJSONInternal(value)))
      case (_, (key, value) :: Nil) ⇒ acc + "," + w(key, toJSONInternal(value))
      case (_, (key, value) :: xs) ⇒ objToJSONRec(xs, acc + "," + w(key, toJSONInternal(value)))
    }
  }

  private[this] def toJSONInternal(v: Ast): String = v match {
    case Ast.Str(x) ⇒ "\"" + x + "\""
    case Ast.Arr(xs) ⇒ "[" + arrayToJSONRec(xs) + "]"
    case Ast.Obj(m) ⇒ "{" + objToJSONRec(m.toList) + "}"
    case Ast.Num(x) ⇒ x.toString
    case Ast.True ⇒ "true"
    case Ast.False ⇒ "false"
    case Ast.Null ⇒ "null"
  }

  def print(ast: Ast): String = toJSONInternal(ast)
}
