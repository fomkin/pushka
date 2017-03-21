package pushka.json

import pushka.{Parser, PushkaException, Ast}

import org.json4s._
import org.json4s.native._
import org.json4s.native.JsonMethods

final class JsonParser extends Parser[String] {
  private[this] def readAst(value: JValue): Ast = value match {
    case JInt(x) => Ast.Num(x.toString)
    case JDecimal(x) => Ast.Num(x.toString)
    case JDouble(x) => Ast.Num(x.toString)
    case JLong(x) => Ast.Num(x.toString)
    case JBool(true) => Ast.True
    case JBool(false) => Ast.False
    case JString(s) => Ast.Str(s)
    case JNull => Ast.Null
    case JObject(v) => Ast.Obj(Map(v.map({ case JField(k, vl) => k -> readAst(vl)}):_*))
    case JArray(items) => Ast.Arr(items.map(readAst(_)).toList)
    case JSet(items) => Ast.Arr(items.map(readAst(_)).toList)
    case JNothing => Ast.Null    
  }

  def parse(data: String): Ast = readAst(JsonMethods.parse(data))
}
