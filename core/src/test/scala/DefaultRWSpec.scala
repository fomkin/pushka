import org.scalatest._
import pushka._

class DefaultRWSpec extends FlatSpec with Matchers with TestKit {

  "Option" should "be read from simple value" in {
    read[Option[String]](Ast.Str("hello")) should be(Option("hello"))
  }

  it should "be written as simple value" in {
    write(Option("hello")) should be(Ast.Str("hello"))
  }
  
  it should "be written as null if it is empty" in {
    write(Option.empty[String]) should be(Ast.Null)
  }
  
  "Int" should "be read from number" in { read[Int](Ast.Num(42)) should be(42) }
  it should "be written to number" in { write(32) should be(Ast.Num(32)) }

  "Float" should "be read from number" in { read[Float](Ast.Num(42)) should be(42f) }
  it should "be written to number" in { write(32f) should be(Ast.Num(32)) }

  "Double" should "be read from number" in { read[Double](Ast.Num(42)) should be(42d) }
  it should "be written to number" in { write(32d) should be(Ast.Num(32)) }

  "Long" should "be read from string" in { read[Long](Ast.Str("42")) should be(42l) }
  it should "be written to string" in { write(32l) should be(Ast.Str("32")) }
  
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

}
