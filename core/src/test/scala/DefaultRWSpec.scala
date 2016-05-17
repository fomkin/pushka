import java.util.UUID

import org.scalatest._
import pushka._

class DefaultRWSpec extends FlatSpec with Matchers {

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
    read[Float](Ast.Num(42)) should be(42f)
  }
  it should "be written to number" in {
    write(32f) should be(Ast.Num(32))
  }
  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast.Null
    val exception = intercept[PushkaException] {
      read[Float](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to Float")
  }

  "Double" should "be read from number" in {
    read[Double](Ast.Num(42)) should be(42d)
  }
  it should "be written to number" in {
    write(32d) should be(Ast.Num(32))
  }
  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast("foo" → "bar")
    val exception = intercept[PushkaException] {
      read[Double](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to Double")
  }

  "Long" should "be read from string" in {
    read[Long](Ast.Str("42")) should be(42l)
  }
  it should "be written to string" in {
    write(32l) should be(Ast.Str("32"))
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

  "Map" should "be written to array of kv pairs" in {
    val source = Map(0 → "a", 1 → "b")
    val pattern = Ast.Arr(List(
      Ast.Arr(List(Ast.Num(0), Ast.Str("a"))),
      Ast.Arr(List(Ast.Num(1), Ast.Str("b")))
    ))
    write(source) shouldEqual pattern
  }
  it should "be read from array of pairs" in {
    val pattern = Map(0 → "a", 1 → "b")
    val source = Ast.Arr(List(
      Ast.Arr(List(Ast.Num(0), Ast.Str("a"))),
      Ast.Arr(List(Ast.Num(1), Ast.Str("b")))
    ))
    read[Map[Int, String]](source) shouldEqual pattern
  }
  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast.Null
    val exception = intercept[PushkaException] {
      read[Map[Int, String]](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to Map")
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
