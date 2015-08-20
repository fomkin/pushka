import org.scalatest._
import pushka.Value
import pushka.json.Unmarshaller

import scala.language.implicitConversions

class UnmarshallerSpec extends FlatSpec with Matchers {

  implicit def m(x: String): Unmarshaller = new Unmarshaller(x)

  "String" should "be unmarshaled" in {
    "\"hello\"".parseJSON shouldEqual Value.Str("hello")
  }

  "Array" should "be unmarshaled" in {
    "[1,2,3]".parseJSON shouldEqual Value.Arr(List(
      Value.Number(1),
      Value.Number(2),
      Value.Number(3)
    ))
  }
}
