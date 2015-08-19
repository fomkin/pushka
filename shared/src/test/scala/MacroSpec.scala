import org.scalatest._
import pushka._

object MacroSpec {
  @pushka case class MyCaseClass(x: Int, y: Int, z: String)
}

class MacroSpec extends FlatSpec with Matchers {
  
  import MacroSpec._
  
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
}
