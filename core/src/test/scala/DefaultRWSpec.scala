import java.util.UUID

import org.scalatest._
import pushka._
import pushka.annotation.pushka

object DefaultRWSpec {

  @pushka case class Id[+T](value: Int)

  implicit def idOk[T] = new ObjectKey[Id[T]] {
    def stringify(x: Id[T]): String = x.value.toString
    def parse(x: String): Id[T] = Id(x.toInt)
  }

  @pushka case class ArbitaryKey(x: Int)
}

class DefaultRWSpec extends FlatSpec with Matchers {

  import DefaultRWSpec._

  "Option" should "be read from simple value" in {
    read[Option[String]](Ast.Str("hello")) should be(Option("hello"))
  }
  it should "be written as simple value" in {
    write(Option("hello")) should be(Ast.Str("hello"))
  }
  it should "be written as null if it is empty" in {
    write(Option.empty[String]) should be(Ast.Null)
  }
  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast.True
    val exception = intercept[PushkaException] {
      read[Option[String]](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to String")
  }

  "Int" should "be read from number" in {
    read[Int](Ast.Num(42)) should be(42)
  }
  it should "be written to number" in {
    write(32) should be(Ast.Num(32))
  }
  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast.False
    val exception = intercept[PushkaException] {
      read[Int](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to Int")
  }

  "Float" should "be read from number" in {
    read[Float](Ast.Num("42.0")) should be(42.0f)
  }
  it should "be written to number" in {
    write[Float](32.0f) should be(Ast.Num(32.0f))
  }
  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast.Null
    val exception = intercept[PushkaException] {
      read[Float](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to Float")
  }

  "Double" should "be read from number" in {
    read[Double](Ast.Num("42.0")) should be(42d)
  }
  it should "be written to number" in {
    write[Double](32.0d) should be(Ast.Num(32.0d))
  }
  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast("foo" → "bar")
    val exception = intercept[PushkaException] {
      read[Double](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to Double")
  }

  "Long" should "be read from string" in {
    read[Long](Ast.Num("609300804742756865")) should be(609300804742756865l)
  }
  it should "be written to string" in {
    write(609300804742756865l) should be(Ast.Num("609300804742756865"))
  }
  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast.Arr(Seq())
    val exception = intercept[PushkaException] {
      read[Long](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to Long")
  }

  "UUID" should "be read from string" in {
    read[UUID](Ast.Str("fff2a55d-afd6-44b9-ba3d-9f87685e6e50")) should be(
      UUID.fromString("fff2a55d-afd6-44b9-ba3d-9f87685e6e50")
    )
  }
  it should "be written to string" in {
    write(UUID.fromString("fff2a55d-afd6-44b9-ba3d-9f87685e6e50")) should be(
      Ast.Str("fff2a55d-afd6-44b9-ba3d-9f87685e6e50")
    )
  }
  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast.True
    val exception = intercept[PushkaException] {
      read[UUID](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to UUID")
  }

  "Seq" should "be read from array" in {
    val source = Ast.Arr(List(Ast.Num(1), Ast.Num(2), Ast.Num(3)))
    val pattern = Seq(1, 2, 3)
    read[Seq[Int]](source) should be(pattern)
  }
  it should "be written to array" in {
    val pattern = Ast.Arr(List(Ast.Num(1), Ast.Num(2), Ast.Num(3)))
    val source = Seq(1, 2, 3)
    write(source) should be(pattern)
  }
  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast.Num(1)
    val exception = intercept[PushkaException] {
      read[Seq[Int]](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to Seq")
  }

  "Array" should "be read from array" in {
    val source = Ast.Arr(List(Ast.Num(1), Ast.Num(2), Ast.Num(3)))
    val pattern = Array(1, 2, 3)
    read[Array[Int]](source) should be(pattern)
  }
  it should "be written to array" in {
    val pattern = Ast.Arr(List(Ast.Num(1), Ast.Num(2), Ast.Num(3)))
    val source = Array(1, 2, 3)
    write(source) should be(pattern)
  }
  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast.Num(1)
    val exception = intercept[PushkaException] {
      read[Array[Int]](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to Array")
  }

  "Map with arbitrary key" should "be written to array of kv pairs" in {
    val source = Map(ArbitaryKey(0) → "a", ArbitaryKey(1) → "b")
    val pattern = Ast.Arr(List(
      Ast.Arr(List(Ast.Num(0), Ast.Str("a"))),
      Ast.Arr(List(Ast.Num(1), Ast.Str("b")))
    ))
    write(source) shouldEqual pattern
  }
  it should "be read from array of pairs" in {
    val pattern = Map(ArbitaryKey(0) → "a", ArbitaryKey(1) → "b")
    val source = Ast.Arr(List(
      Ast.Arr(List(Ast.Num(0), Ast.Str("a"))),
      Ast.Arr(List(Ast.Num(1), Ast.Str("b")))
    ))
    read[Map[ArbitaryKey, String]](source) shouldEqual pattern
  }
  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast.Null
    val exception = intercept[PushkaException] {
      read[Map[ArbitaryKey, String]](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to Map")
  }

  "Map which key could be converted to string by default" should "be written to JSON object" in {
    val source = Map("0" → "a", "1" → "b")
    val pattern = Ast.Obj(Map("0" → Ast.Str("a"), "1" → Ast.Str("b")))
    write(source) shouldEqual pattern
  }
  it should "be read from JSON object" in {
    val pattern = Map(
      UUID.fromString("102b392e-862d-45e4-8140-35493598dff7") → "a",
      UUID.fromString("2c2fe2ce-1f6d-4cca-8ade-68a7bedc226f") → "b")
    val source = Ast.Obj(Map(
      "102b392e-862d-45e4-8140-35493598dff7" → Ast.Str("a"),
      "2c2fe2ce-1f6d-4cca-8ade-68a7bedc226f" → Ast.Str("b"))
    )
    read[Map[UUID, String]](source) shouldEqual pattern
  }

  "Map which custom key could be converted to string" should "be written to JSON object" in {
    val source = Map(Id[ArbitaryKey](0) → "a", Id[ArbitaryKey](1) → "b")
    val pattern = Ast.Obj(Map("0" → Ast.Str("a"), "1" → Ast.Str("b")))
    write(source) shouldEqual pattern
  }
  it should "be read from JSON object" in {
    val pattern = Map(Id[ArbitaryKey](0) → "a", Id[ArbitaryKey](1) → "b")
    val source = Ast.Obj(Map("0" → Ast.Str("a"), "1" → Ast.Str("b")))
    read[Map[Id[ArbitaryKey], String]](source) shouldEqual pattern
  }

  "Set" should "be read from array" in {
    val source = Ast.Arr(Vector(Ast.Num(1), Ast.Num(2), Ast.Num(3)))
    val pattern = Set(1, 2, 3)
    read[Set[Int]](source) should be(pattern)
  }
  it should "be written into array" in {
    val pattern = Ast.Arr(Vector(Ast.Num(1), Ast.Num(2), Ast.Num(3)))
    val source = Set(1, 2, 3)
    write(source) should be(pattern)
  }
  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast("bar" → "foo")
    val exception = intercept[PushkaException] {
      read[Set[Int]](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to Set")
  }

  "Either left" should "be read from object" in {
    val source = Ast.Obj(Map("left" → Ast.Str("hello")))
    val pattern = Left("hello")
    read[Either[String, Int]](source) should be(pattern)
  }
  it should "be written into object" in {
    val source = Left("hello")
    val pattern = Ast.Obj(Map("left" → Ast.Str("hello")))
    write[Either[String, Int]](source) should be(pattern)
  }
  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast.Arr(Seq())
    val exception = intercept[PushkaException] {
      read[Either[String, Int]](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to Either")
  }

  "Either right" should "be read from object" in {
    val source = Ast.Obj(Map("right" → Ast.Num(20)))
    val pattern = Right(20)
    read[Either[String, Int]](source) should be(pattern)
  }
  it should "be written into object" in {
    val source = Right(20)
    val pattern = Ast.Obj(Map("right" → Ast.Num(20)))
    write[Either[String, Int]](source) should be(pattern)
  }
  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast.False
    val exception = intercept[PushkaException] {
      read[Either[String, Int]](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to Either")
  }

}
