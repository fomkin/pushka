import org.scalatest._
import pushka._

object CaseClassSpec {
  @pushka case class MyCaseClass(x: Int, y: Int, z: String)
  @pushka case class MyCaseClass2(x: Option[String], y: String)
}

class CaseClassSpec extends FlatSpec with Matchers {
  
  import CaseClassSpec._
  
  "Case classes" should "writes correctly" in {
    val instance = MyCaseClass(10, 10, "vodka")
    val m = Map(
      "x" → pushka.Value.Number(10), 
      "y" → pushka.Value.Number(10),
      "z" → pushka.Value.Str("vodka")
    )
    write(instance) should be(pushka.Value.Obj(m))
  }

  it should "reads correctly" in {
    val instance = MyCaseClass(10, 10, "vodka")
    val m = Map(
      "x" → pushka.Value.Number(10),
      "y" → pushka.Value.Number(10),
      "z" → pushka.Value.Str("vodka")
    )
    read[MyCaseClass](pushka.Value.Obj(m)) should be(instance)
  }
  
  "Option fields" should "be write without overhead" in {
    val pattern = Value.Obj(Map("y" → Value.Str("vodka")))
    write(MyCaseClass2(None, "vodka")) should be(pattern)
  }

  "Option fields" should "be read without overhead" in {
    val source = Value.Obj(Map("y" → Value.Str("vodka")))
    val pattern = MyCaseClass2(None, "vodka")
    read[MyCaseClass2](source) should be(pattern)
  }
}
