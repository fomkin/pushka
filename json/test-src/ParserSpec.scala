import org.scalatest._
import pushka.Ast
import pushka.json.JsonParser

import scala.language.implicitConversions

class ParserSpec extends FlatSpec with Matchers {

  implicit val parser = new JsonParser()

  "String" should "be unmarshaled" in {
    parser.parse("\"hello\"") shouldEqual Ast.Str("hello")
  }

  "Array" should "be unmarshaled" in {
    parser.parse("[1,2,3]") shouldEqual Ast.Arr(List(
      Ast.Num("1"),
      Ast.Num("2"),
      Ast.Num("3")
    ))
  }
}
