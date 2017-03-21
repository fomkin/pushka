package pushka

import pushka._
import pushka.json._
import pushka.annotation._

// import scala.util.{Try, Success, Failure}
import scala.annotation.StaticAnnotation

/**
 * Commented out test cases due to:
 * [error] cannot link: @java.util.regex.Pattern
 * [error] cannot link: @java.util.regex.Pattern$
 * [error] cannot link: @java.util.regex.Pattern$::compile_class.java.lang.String_class.java.util.regex.Pattern
 * [error] cannot link: @java.util.regex.Pattern::split_trait.java.lang.CharSequence_class.ssnr.ObjectArray
 * [error] unable to link
*/
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

  def run: Unit = {
    {
      val instance = Container(User.Name("John", "Doe"), 10)
      assert(write(instance) == 
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

    {
      val instance = Container(User.Empty, 10)
      assert(write(instance) == 
        Ast.Obj(Map(
          "user" → Ast.Str("empty"),
          "anotherField" → Ast.Num("10"
          ))
        )
      )
    }

    {
      assert(write[WithBody](WithBody.A) == Ast.Str("a"))
      assert(write[WithBody](WithBody.B(1)) == Ast.Obj(Map("b" → Ast.Num(1))))
    }

    {
      write[Base](Base.Descendant(42))
    }

    /*{
      assert(read[Color](Ast.Str("red")) == Color.Red)
    }

    {
      val invalidAst = Ast("foo" → "bar")
      val exception = Try { read[Color](invalidAst) }
      // exception.message should be(s"Error while reading AST $invalidAst to Color")
    }*/
  }
}