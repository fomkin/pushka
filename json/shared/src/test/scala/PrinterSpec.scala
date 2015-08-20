import org.scalatest._
import pushka.{Ast}
import pushka.json.JsonPrinter

import scala.language.implicitConversions

class PrinterSpec extends FlatSpec with Matchers {

  implicit val printer = new JsonPrinter()

  "String" should "be printed" in {
    printer.print(Ast.Str("hello")) should be("\"hello\"")
  }

  // TODO different behavior between JVM and JS
  //  "Number" should "be printed" in {
  //    Value.Number(42).toJSON should be("42.0")
  //  }

  "True" should "be printed" in {
    printer.print(Ast.True) should be("true")
  }

  "False" should "be printed" in {
    printer.print(Ast.False) should be("false")
  }

  "Null" should "be printed" in {
    printer.print(Ast.Null) should be("null")
  }

  "Obj" should "be printed" in {
    val source = Ast.Obj(Map("x" → Ast.Null, "y" → Ast.Null))
    printer.print(source) shouldEqual "{\"x\":null,\"y\":null}"
  }

  "Arr" should "be printed" in {
    val source = Ast.Arr(List(Ast.Str("1"), Ast.Str("2"), Ast.Str("3")))
    printer.print(source) should be("[\"1\",\"2\",\"3\"]")
  }
}
