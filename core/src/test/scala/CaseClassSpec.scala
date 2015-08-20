import org.scalatest._
import pushka._
import pushka.annotation.pushka

object CaseClassSpec {
  @pushka case class MyCaseClass(x: Int, y: Int, z: String)
  @pushka case class MyCaseClass2(x: Option[String], y: String)
  @pushka case class Id[+T](x: Int)
}

class CaseClassSpec extends FlatSpec with Matchers with TestKit {
  
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

  "Case classes with one filed" should "be read as value" in {
    val source = Ast.Num(10)
    read[Id[String]](source) should be(Id[String](10))
  }

  "Option fields" should "be write without overhead" in {
    val pattern = Ast.Obj(Map("y" → Ast.Str("vodka")))
    write[MyCaseClass2](MyCaseClass2(None, "vodka")) should be(pattern)
  }

  "Option fields" should "be read without overhead" in {
    val source = Ast.Obj(Map("y" → Ast.Str("vodka")))
    val pattern = MyCaseClass2(None, "vodka")
    read[MyCaseClass2](source) should be(pattern)
  }
}
