import org.scalatest._
import pushka.Value
import pushka.json._

class MarshallerSpec extends FlatSpec with Matchers {

  "String" should "be marshaled" in {
    Value.Str("hello").toJSON should be("\"hello\"")
  }

// TODO different behavior between JVM and JS
//  "Number" should "be marshaled" in {
//    Value.Number(42).toJSON should be("42.0")
//  }

  "True" should "be marshaled" in {
    Value.Bool(value = true).toJSON should be("true")
  }

  "False" should "be marshaled" in {
    Value.Bool(value = false).toJSON should be("false")
  }

  "Null" should "be marshaled" in {
    Value.Null.toJSON should be("null")
  }

  "Obj" should "be marshaled" in {
    val source = Value.Obj(Map("x" → Value.Null, "y" → Value.Null))
    source.toJSON shouldEqual "{\"x\"=null,\"y\"=null}"
  }

  "Arr" should "be marshaled" in {
    val source = Value.Arr(List(Value.Str("1"), Value.Str("2"), Value.Str("3")))
    source.toJSON should be("[\"1\",\"2\",\"3\"]")
  }
}
