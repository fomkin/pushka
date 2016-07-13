import org.scalatest._
import pushka._
import pushka.annotation._

object CaseClassSpec {
  @pushka case class MyCaseClass(x: Int, y: Int, z: String)
  @pushka case class MyCaseClass2(x: Option[String], y: String)
  @pushka case class MyCaseClass3(x: (String, Double), y: String)
  @pushka case class Id[+T](x: Int)
  @pushka case class Point[T](x: T, y: T)
  @pushka case class WithDefaultParams(x: Int, y: Int = 100)
  @pushka case class WithKeyAnnotation(@key("@theX") x: Int, y: Int)
  @pushka @forceObject case class AppleReceipt(@key("receipt-data") receiptData: String)
}

class CaseClassSpec extends FlatSpec with Matchers {

  import CaseClassSpec._

  "Case classes" should "writes correctly" in {
    val instance = MyCaseClass(10, 10, "vodka")
    val m = Map(
      "x" → pushka.Ast.Num(10),
      "y" → pushka.Ast.Num(10),
      "z" → pushka.Ast.Str("vodka")
    )
    write(instance) should be(pushka.Ast.Obj(m))
  }

  it should "reads correctly" in {
    val instance = MyCaseClass(10, 10, "vodka")
    val m = Map(
      "x" → pushka.Ast.Num(10),
      "y" → pushka.Ast.Num(10),
      "z" → pushka.Ast.Str("vodka")
    )
    read[MyCaseClass](pushka.Ast.Obj(m)) should be(instance)
  }

  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast.Arr(Seq())
    val exception = intercept[PushkaException] {
      read[MyCaseClass](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to MyCaseClass")
  }

  it should "throw exception with correct message if some fields are not defined" in {
    val invalidAst = Ast(
      "x" → pushka.Ast.Num(10),
      "z" → pushka.Ast.Str("vodka")
    )
    val exception = intercept[PushkaException] {
      read[MyCaseClass](invalidAst)
    }

    exception.message should be(s"MyCaseClass should contain y")
  }

  "Case classes with one filed" should "be written as value" in {
    val source = Id[String](10)
    write[Id[String]](source) should be(Ast.Num(10))
  }

  it should "be read as value" in {
    val source = Ast.Num(10)
    read[Id[String]](source) should be(Id[String](10))
  }

  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast.True
    val exception = intercept[PushkaException] {
      read[Id[String]](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to Int")
  }

  "Generic case class" should "be written" in {
    val source = Point[Float](10, 10)
    val pattern = Ast("x" → 10.0, "y" → 10.0)
    write(source) shouldEqual pattern
  }

  it should "be read" in {
    val source = Ast("x" → 10.0, "y" → 10.0)
    val pattern = Point[Float](10, 10)
    read[Point[Float]](source) shouldEqual pattern
  }

  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast.Null
    val exception = intercept[PushkaException] {
      read[Point[Float]](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to Point")
  }

  "Option fields" should "be written without overhead" in {
    val pattern = Ast.Obj(Map("y" → Ast.Str("vodka")))
    write[MyCaseClass2](MyCaseClass2(None, "vodka")) should be(pattern)
  }

  it should "be read as None when field is not defined" in {
    val source = Ast.Obj(Map("y" → Ast.Str("vodka")))
    val pattern = MyCaseClass2(None, "vodka")
    read[MyCaseClass2](source) should be(pattern)
  }

  it should "be read as None when field is null" in {
    val source = Ast.Obj(Map("x" → Ast.Null, "y" → Ast.Str("vodka")))
    val pattern = MyCaseClass2(None, "vodka")
    read[MyCaseClass2](source) should be(pattern)
  }

  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast.Num(5)
    val exception = intercept[PushkaException] {
      read[MyCaseClass2](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to MyCaseClass2")
  }

  "Tuple fields" should "be written correctly" in {
    val pattern = Ast.Obj(Map(
      "x" → Ast.Arr(Seq(
        Ast.Str("bear"),
        Ast.Num(9)
      )),
      "y" → Ast.Str("vodka"))
    )
    write(MyCaseClass3(("bear", 9), "vodka")) should be(pattern)
  }

  it should "be read" in {
    val source = Ast.Obj(Map(
      "x" → Ast.Arr(Seq(
        Ast.Str("bear"),
        Ast.Num(9)
      )),
      "y" → Ast.Str("vodka"))
    )
    val pattern = MyCaseClass3(("bear", 9), "vodka")
    read[MyCaseClass3](source) should be(pattern)
  }

  "None" should "be written as null when leanOptions is switched off" in {
    implicit val config = pushka.Config(leanOptions = false)
    val pattern = Ast.Obj(Map("x" → Ast.Null, "y" → Ast.Str("vodka")))
    write[MyCaseClass2](MyCaseClass2(None, "vodka")) should be(pattern)
  }

  "Case class with default params" should "be read with default parameter if it was not defined in AST" in {
    val source = Ast.Obj(Map("x" → Ast.Num(1)))
    val pattern = WithDefaultParams(1, 100)
    read[WithDefaultParams](source) shouldEqual pattern
  }

  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast.False
    val exception = intercept[PushkaException] {
      read[WithDefaultParams](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to WithDefaultParams")
  }

  "Case class with @key" should "be written" in {
    val pattern = Ast.Obj(Map("@theX" → Ast.Num(1), "y" → Ast.Num(2)))
    write(WithKeyAnnotation(1, 2)) should be(pattern)
  }

  it should "be read" in {
    val source = Ast.Obj(Map("@theX" → Ast.Num(1), "y" → Ast.Num(2)))
    val pattern = WithKeyAnnotation(1, 2)
    read[WithKeyAnnotation](source) should be(pattern)
  }

  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast.True
    val exception = intercept[PushkaException] {
      read[WithKeyAnnotation](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to WithKeyAnnotation")
  }

  "Case class annotated with @forceObject and having one field" should "be written as object" in {
    val source = AppleReceipt("hello")
    val pattern = Ast("receipt-data" -> "hello")
    write(source) should be (pattern)
  }

  it should "be read from object" in {
    val pattern = AppleReceipt("hello")
    val source = Ast("receipt-data" -> "hello")
    read[AppleReceipt](source) should be (pattern)
  }
}
