import org.scalatest._
import pushka._

class DefaultRWSpec extends FlatSpec with Matchers {

  "Option" should "be read from simple value" in {
    read[Option[String]](Value.Str("hello")) should be(Option("hello"))
  }

  it should "be written as simple value" in {
    write(Option("hello")) should be(Value.Str("hello"))
  }
  
  it should "be written as null if it is empty" in {
    write(Option.empty[String]) should be(Value.Null)
  }
  
  "Int" should "be read from number" in { read[Int](Value.Number(42)) should be(42) }
  it should "be written to number" in { write(32) should be(Value.Number(32)) }

  "Float" should "be read from number" in { read[Float](Value.Number(42)) should be(42f) }
  it should "be written to number" in { write(32f) should be(Value.Number(32)) }

  "Double" should "be read from number" in { read[Double](Value.Number(42)) should be(42d) }
  it should "be written to number" in { write(32d) should be(Value.Number(32)) }

  "Long" should "be read from string" in { read[Long](Value.Str("42")) should be(42l) }
  it should "be written to string" in { write(32l) should be(Value.Str("32")) }
}
