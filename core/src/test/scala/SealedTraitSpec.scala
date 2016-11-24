import org.scalatest._
import pushka._
import pushka.annotation.pushka

import scala.annotation.StaticAnnotation

object SealedTraitSpec {

  abstract class Rgb(r: Int, g: Int, b: Int)

  @pushka sealed trait Color

  object Color {
    case object Red extends Rgb(255, 0, 0) with Color
    case object Green extends Rgb(0, 255, 0) with Color
    case object Blue extends Rgb(0, 0, 255) with Color
  }

  @pushka sealed trait WithBody {
    def x: Int
  }

  object WithBody {
    case object A extends WithBody { val x = 0 }
    case class B(y: Int) extends WithBody { val x = 1 }
  }

  @pushka sealed trait User

  object User {
    case object Empty extends User
    case class Name(first: String, last: String) extends User
    case object Anonymous extends User
  }

  @pushka case class Container(user: User, anotherField: Int)

  class theAnnotation(a: String, b: String) extends StaticAnnotation

  @pushka sealed trait Base

  object Base {
    @theAnnotation("Some message", "Today")
    case class Descendant(value: Int) extends Base

    object Descendant {
      def haha(): Unit = {}
    }
  }
}

class SealedTraitSpec extends FlatSpec with Matchers {

  import SealedTraitSpec._

  "ADT based on case class" should "writes by case class rules in property" in {
    val instance = Container(User.Name("John", "Doe"), 10)
    write(instance) should be(
      Ast.Obj(Map(
        "user" → Ast.Obj(Map(
          "name" → Ast.Obj(Map(
            "first" → Ast.Str("John"),
            "last" → Ast.Str("Doe"))
          ))),
        "anotherField" → Ast.Num("10")
      ))
    )
  }

  "ADT based on case object" should "writes as simple string" in {
    val instance = Container(User.Empty, 10)
    write(instance) should be(
      Ast.Obj(Map(
        "user" → Ast.Str("empty"),
        "anotherField" → Ast.Num("10"
        ))
      )
    )
  }

  "Sealed trait with body" should "be processed" in {
    write[WithBody](WithBody.A) shouldEqual Ast.Str("a")
    write[WithBody](WithBody.B(1)) shouldEqual Ast.Obj(Map("b" → Ast.Num(1)))
  }

  "Deprecated annotation in case classes" should "not breaks writing" in {
    write[Base](Base.Descendant(42))
  }

  "Color" should "be read correctly" in {
    read[Color](Ast.Str("red")) shouldBe Color.Red
  }

  it should "throw exception with correct message if Ast is invalid" in {
    val invalidAst = Ast("foo" → "bar")
    val exception = intercept[PushkaException] {
      read[Color](invalidAst)
    }
    exception.message should be(s"Error while reading AST $invalidAst to Color")
  }
}
