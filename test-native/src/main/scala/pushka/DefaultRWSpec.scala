package pushka

import pushka.annotation.pushka

import scala.util.Try
import java.util.UUID

object DefaultRWSpec extends TestMethods {

  @pushka case class Id[+T](value: Int)

  implicit def idOk[T] = new ObjectKey[Id[T]] {
    def stringify(x: Id[T]): String = x.value.toString
    def parse(x: String): Id[T] = Id(x.toInt)
  }

  @pushka case class ArbitaryKey(x: Int)

  def run: Unit = {
    {
      assert(read[Option[String]](Ast.Str("hello")) == Option("hello"))
    }

    {
      assert(write(Option.empty[String]) == Ast.Null)
    }

    {
      val invalidAst = Ast.True
      val exception = Try { read[Option[String]](invalidAst) }
      exception.exceptionAssert(s"Error while reading AST $invalidAst to String")
    }

    {
      assert(read[Int](Ast.Num(42)) == 42)
    }

    {
      assert(write(32) == Ast.Num(32))
    }

    {
      val invalidAst = Ast.False
      val exception = Try { read[Int](invalidAst) }
      exception.exceptionAssert(s"Error while reading AST $invalidAst to Int")
    }

    {
      assert(read[Float](Ast.Num("42.0")) == 42.0f)
    }

    {
      assert(write[Float](32.0f) == Ast.Num(32.0f))
    }

    {
      val invalidAst = Ast.Null
      val exception = Try { read[Float](invalidAst) }
      exception.exceptionAssert(s"Error while reading AST $invalidAst to Float")
    }

    {
      assert(read[Double](Ast.Num("42.0")) == 42d)
    }

    {
      assert(write[Double](32.0d) == Ast.Num(32.0d))
    }
    {
      val invalidAst = Ast("foo" → "bar")
      val exception = Try { read[Double](invalidAst) }
      exception.exceptionAssert(s"Error while reading AST $invalidAst to Double")
    }

    {
      assert(read[Long](Ast.Num("609300804742756865")) == 609300804742756865l)
    }
    {
      assert(write(609300804742756865l) == Ast.Num("609300804742756865"))
    }
    {
      val invalidAst = Ast.Arr(Seq())
      val exception = Try { read[Long](invalidAst) }
      exception.exceptionAssert(s"Error while reading AST $invalidAst to Long")
    }

    {
      assert(read[UUID](Ast.Str("fff2a55d-afd6-44b9-ba3d-9f87685e6e50")) == UUID.fromString("fff2a55d-afd6-44b9-ba3d-9f87685e6e50"))     
    }
    {
      assert(write(UUID.fromString("fff2a55d-afd6-44b9-ba3d-9f87685e6e50")) == Ast.Str("fff2a55d-afd6-44b9-ba3d-9f87685e6e50"))
    }
    {
      val invalidAst = Ast.True
      val exception = Try { read[UUID](invalidAst) }
      exception.exceptionAssert(s"Error while reading AST $invalidAst to UUID")
    }

    {
      val source = Ast.Arr(List(Ast.Num(1), Ast.Num(2), Ast.Num(3)))
      val pattern = Seq(1, 2, 3)
      assert(read[Seq[Int]](source) == pattern)
    }
    {
      val pattern = Ast.Arr(List(Ast.Num(1), Ast.Num(2), Ast.Num(3)))
      val source = Seq(1, 2, 3)
      assert(write(source) == pattern)
    }

    {
      val invalidAst = Ast.Num(1)
      val exception = Try { read[Seq[Int]](invalidAst) }
      exception.exceptionAssert(s"Error while reading AST $invalidAst to Seq")
    }

    {
      val source = Ast.Arr(List(Ast.Num(1), Ast.Num(2), Ast.Num(3)))
      val pattern = Array(1, 2, 3)
      assert(read[Array[Int]](source) sameElements pattern)
    }

    {
      val pattern = Ast.Arr(List(Ast.Num(1), Ast.Num(2), Ast.Num(3)))
      val source = Array(1, 2, 3)
      assert(write(source) == pattern)
    }
    {
      val invalidAst = Ast.Num(1)
      val exception = Try { read[Array[Int]](invalidAst) }
      exception.exceptionAssert(s"Error while reading AST $invalidAst to Array")
    }

    {
      val source = Map(ArbitaryKey(0) → "a", ArbitaryKey(1) → "b")
      val pattern = Ast.Arr(List(
        Ast.Arr(List(Ast.Num(0), Ast.Str("a"))),
        Ast.Arr(List(Ast.Num(1), Ast.Str("b")))
      ))
      assert(write(source) == pattern)
    }
    {
      val pattern = Map(ArbitaryKey(0) → "a", ArbitaryKey(1) → "b")
      val source = Ast.Arr(List(
        Ast.Arr(List(Ast.Num(0), Ast.Str("a"))),
        Ast.Arr(List(Ast.Num(1), Ast.Str("b")))
      ))
      assert(read[Map[ArbitaryKey, String]](source) == pattern)
    }
    {
      val invalidAst = Ast.Null
      val exception = Try { read[Map[ArbitaryKey, String]](invalidAst) }
      exception.exceptionAssert(s"Error while reading AST $invalidAst to Map")
    }

    {
      val source = Map("0" → "a", "1" → "b")
      val pattern = Ast.Obj(Map("0" → Ast.Str("a"), "1" → Ast.Str("b")))
      assert(write(source) == pattern)
    }

    {
      val pattern = Map(
        UUID.fromString("102b392e-862d-45e4-8140-35493598dff7") → "a",
        UUID.fromString("2c2fe2ce-1f6d-4cca-8ade-68a7bedc226f") → "b")
      val source = Ast.Obj(Map(
        "102b392e-862d-45e4-8140-35493598dff7" → Ast.Str("a"),
        "2c2fe2ce-1f6d-4cca-8ade-68a7bedc226f" → Ast.Str("b"))
      )
      assert(read[Map[UUID, String]](source) == pattern)
    }

    {
      val source = Map(Id[ArbitaryKey](0) → "a", Id[ArbitaryKey](1) → "b")
      val pattern = Ast.Obj(Map("0" → Ast.Str("a"), "1" → Ast.Str("b")))
      assert(write(source) == pattern)
    }

    {
      val pattern = Map(Id[ArbitaryKey](0) → "a", Id[ArbitaryKey](1) → "b")
      val source = Ast.Obj(Map("0" → Ast.Str("a"), "1" → Ast.Str("b")))
      assert(read[Map[Id[ArbitaryKey], String]](source) == pattern)
    }

    {
      val source = Ast.Arr(Vector(Ast.Num(1), Ast.Num(2), Ast.Num(3)))
      val pattern = Set(1, 2, 3)
      assert(read[Set[Int]](source) == pattern)
    }

    {
      val pattern = Ast.Arr(Vector(Ast.Num(1), Ast.Num(2), Ast.Num(3)))
      val source = Set(1, 2, 3)
      assert(write(source) == pattern)
    }

    {
      val invalidAst = Ast("bar" → "foo")
      val exception = Try { read[Set[Int]](invalidAst) }
      exception.exceptionAssert(s"Error while reading AST $invalidAst to Set")
    }

    {
      val source = Ast.Obj(Map("left" → Ast.Str("hello")))
      val pattern = Left("hello")
      assert(read[Either[String, Int]](source) == pattern)
    }

    {
      val source = Left("hello")
      val pattern = Ast.Obj(Map("left" → Ast.Str("hello")))
      assert(write[Either[String, Int]](source) == pattern)
    }

    {
      val invalidAst = Ast.Arr(Seq())
      val exception = Try { read[Either[String, Int]](invalidAst) }
      exception.exceptionAssert(s"Error while reading AST $invalidAst to Either")
    }

    {
      val source = Ast.Obj(Map("right" → Ast.Num(20)))
      val pattern = Right(20)
      assert(read[Either[String, Int]](source) == pattern)
    }

    {
      val source = Right(20)
      val pattern = Ast.Obj(Map("right" → Ast.Num(20)))
      assert(write[Either[String, Int]](source) == pattern)
    }

    {
      val invalidAst = Ast.False
      val exception = Try { read[Either[String, Int]](invalidAst) }
      exception.exceptionAssert(s"Error while reading AST $invalidAst to Either")
    }
  }
}