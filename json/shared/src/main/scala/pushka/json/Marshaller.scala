package pushka.json

import pushka.Value

import scala.annotation.tailrec

final class Marshaller(val self: Value) extends AnyVal {

  @tailrec
  private[this] def arrayToJSONRec(tail: List[Value], acc: String = ""): String = (acc, tail) match {
    case (_, Nil) ⇒ acc
    case ("", x :: Nil) ⇒ toJSONInternal(x)
    case ("", x :: xs) ⇒ arrayToJSONRec(xs, toJSONInternal(x))
    case (_, x :: Nil) ⇒ acc + "," + toJSONInternal(x)
    case (_, x :: xs) ⇒ arrayToJSONRec(xs, acc + "," + toJSONInternal(x))
  }

  @tailrec
  private[this] def objToJSONRec(tail: List[(String, Value)], acc: String = ""): String = {
    @inline def w(key: String, value: String) = "\"" + key + "\":" + value
    (acc, tail) match {
      case (_, Nil) ⇒ acc
      case ("", (key, value) :: Nil) ⇒ w(key, toJSONInternal(value))
      case ("", (key, value) :: xs) ⇒ objToJSONRec(xs, w(key, toJSONInternal(value)))
      case (_, (key, value) :: Nil) ⇒ acc + "," + w(key, toJSONInternal(value))
      case (_, (key, value) :: xs) ⇒ objToJSONRec(xs, acc + "," + w(key, toJSONInternal(value)))
    }
  }

  private[this] def toJSONInternal(v: Value): String = v match {
    case Value.Str(x) ⇒ "\"" + x + "\""
    case Value.Arr(xs) ⇒ "[" + arrayToJSONRec(xs) + "]"
    case Value.Obj(m) ⇒ "{" + objToJSONRec(m.toList) + "}"
    case Value.Number(x) ⇒ x.toString
    case Value.True ⇒ "true"
    case Value.False ⇒ "false"
    case Value.Null ⇒ "null"
  }

  def toJSON: String = toJSONInternal(self)
}
