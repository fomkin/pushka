import org.scalatest._
import pushka._
import pushka.annotation.pushka

object CaseClassSpec {
  @pushka case class MyCaseClass(x: Int, y: Int, z: String)
  @pushka case class MyCaseClass2(x: Option[String], y: String)
  @pushka case class Id[+T](x: Int)
  @pushka case class Point[T](x: T, y: T)
  @pushka case class WithDefaultParams(x: Int, y: Int = 100)
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

  "Case classes with one filed" should "be written as value" in {
    val source = Id[String](10)
    write[Id[String]](source) should be(Ast.Num(10))
  }

  it should "be read as value" in {
    val source = Ast.Num(10)
    read[Id[String]](source) should be(Id[String](10))
  }

  "Generic case class" should "be written" in {
    val source = Point[Float](10, 10)
    val pattern = Ast("x" → 10.0, "y" → 10.0)
    write(source) shouldEqual pattern
  }

  "Generic case class" should "be read" in {
    val source = Ast("x" → 10.0, "y" → 10.0)
    val pattern = Point[Float](10, 10)
    read[Point[Float]](source) shouldEqual pattern
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
}
