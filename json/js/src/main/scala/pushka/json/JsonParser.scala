package pushka.json

import pushka.{Parser, PushkaException, Ast}

import scala.scalajs.js
import scala.scalajs.js.JSON

final class JsonParser extends Parser[String] {

  private[this] def readAst(value: Any): Ast = value match{
    case s: String => Ast.Str(s)
    case n: Double => Ast.Num(n.toString)
    case true => Ast.True
    case false => Ast.False
    case null => Ast.Null
    case s: js.Array[_] => Ast.Arr(s.map(readAst(_: Any)).toList)
    case s: js.Object => Ast.Obj(s.asInstanceOf[js.Dictionary[_]].mapValues(readAst).toMap)
  }

  def parse(data: String): Ast = {
    val parsed = try {
      JSON.parse(data)
    } catch{ case js.JavaScriptException(e: js.SyntaxError) =>
      throw PushkaException(s"Error while parsing JSON $data: ${e.message}")
    }
    readAst(parsed)
  }

}
