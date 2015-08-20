package pushka.json

import pushka.{PushkaException, Value}

import scala.scalajs.js
import scala.scalajs.js.JSON

final class Unmarshaller(val self: String) extends AnyVal {

  def readJs(value: Any): Value = value match{
    case s: String => Value.Str(s)
    case n: Double => Value.Number(n)
    case true => Value.True
    case false => Value.False
    case null => Value.Null
    case s: js.Array[_] => Value.Arr(s.map(readJs(_: Any)).toList)
    case s: js.Object => Value.Obj(s.asInstanceOf[js.Dictionary[_]].mapValues(readJs).toMap)
  }

  def parseJSON: Value = {
    val parsed = try {
      JSON.parse(self)
    } catch{ case js.JavaScriptException(e: js.SyntaxError) =>
      throw PushkaException(Some(e.message))
    }
    readJs(parsed)
  }

}
