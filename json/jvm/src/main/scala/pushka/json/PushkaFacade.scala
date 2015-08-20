package pushka.json

import jawn.SimpleFacade
import pushka.Value

object PushkaFacade extends SimpleFacade[Value] {
  
  def jarray(vs: List[Value]): Value = Value.Arr(vs)

  def jobject(vs: Map[String, Value]): Value = Value.Obj(vs)

  def jint(s: String): Value = Value.Number(s.toDouble)

  def jfalse(): Value = Value.False

  def jnum(s: String): Value = Value.Number(s.toDouble)

  def jnull(): Value = Value.Null

  def jtrue(): Value = Value.True

  def jstring(s: String): Value = Value.Str(s)
}
