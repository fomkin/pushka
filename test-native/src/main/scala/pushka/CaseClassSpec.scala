package pushka

import pushka.annotation._

// import scala.util.{Try, Success, Failure}

/**
 * Commented out test cases due to:
 * [error] cannot link: @java.util.regex.Pattern
 * [error] cannot link: @java.util.regex.Pattern$
 * [error] cannot link: @java.util.regex.Pattern$::compile_class.java.lang.String_class.java.util.regex.Pattern
 * [error] cannot link: @java.util.regex.Pattern::split_trait.java.lang.CharSequence_class.ssnr.ObjectArray
 * [error] unable to link
*/
object CaseClassSpec {
  @pushka case class MyCaseClass(x: Int, y: Int, z: String)
  @pushka case class MyCaseClass2(x: Option[String], y: String)
  @pushka case class MyCaseClass3(x: (String, Double), y: String)
  @pushka case class Id[+T](x: Int)
  @pushka case class Point[T](x: T, y: T)
  @pushka case class WithDefaultParams(x: Int, y: Int = 100, z: Option[Point[Int]] = Some(Point(1, 1)))
  @pushka case class WithKeyAnnotation(@key("@theX") x: Int, y: Int)
  @pushka @forceObject case class AppleReceipt(@key("receipt-data") receiptData: String)

  def run: Unit = {
    {
      val instance = MyCaseClass(10, 10, "vodka")
      val m = Map(
        "x" → pushka.Ast.Num(10),
        "y" → pushka.Ast.Num(10),
        "z" → pushka.Ast.Str("vodka")
      )
      assert(write(instance) == pushka.Ast.Obj(m))
    }

    /*{
      val invalidAst = Ast.Arr(Seq())
      val exception = Try { read[MyCaseClass](invalidAst) } 
      //exception.message should be(s"Error while reading AST $invalidAst to MyCaseClass")
    }

    {
      val invalidAst = Ast(
        "x" → pushka.Ast.Num(10),
        "z" → pushka.Ast.Str("vodka")
      )
      val exception = Try { read[MyCaseClass](invalidAst) }
      //exception.message should be(s"MyCaseClass should contain y")
    }*/

    {
      val source = Id[String](10)
      assert(write[Id[String]](source) == Ast.Num(10))
    }

    /*{
      val source = Ast.Num(10)
      assert(read[Id[String]](source) == Id[String](10))
    }*/

    /*{
      val invalidAst = Ast.True
      val exception = Try { read[Id[String]](invalidAst) }
      // exception.message should be(s"Error while reading AST $invalidAst to Int")
    }*/

    {
      val source = Point[Float](10, 10)
      val pattern = Ast("x" → 10.0, "y" → 10.0)
      assert(write(source) == pattern)
    }

    /*{
      val source = Ast("x" → 10.0, "y" → 10.0)
      val pattern = Point[Float](10, 10)
      assert(read[Point[Float]](source) == pattern)
    }*/

    /*{
      val invalidAst = Ast.Null
      val exception = Try { read[Point[Float]](invalidAst) }
      // exception.message should be(s"Error while reading AST $invalidAst to Point")
    }*/

    {
      val pattern = Ast.Obj(Map("y" → Ast.Str("vodka")))
      assert(write[MyCaseClass2](MyCaseClass2(None, "vodka")) == pattern)
    }

    /*{
      val source = Ast.Obj(Map("y" → Ast.Str("vodka")))
      val pattern = MyCaseClass2(None, "vodka")
      assert(read[MyCaseClass2](source) == pattern)
    }

    {
      val source = Ast.Obj(Map("x" → Ast.Null, "y" → Ast.Str("vodka")))
      val pattern = MyCaseClass2(None, "vodka")
      assert(read[MyCaseClass2](source) == pattern)
    }

    {
      val invalidAst = Ast.Num(5)
      val exception = Try { read[MyCaseClass2](invalidAst) }
      //exception.message should be(s"Error while reading AST $invalidAst to MyCaseClass2")
    }*/

    {
      val pattern = Ast.Obj(Map(
        "x" → Ast.Arr(Seq(
          Ast.Str("bear"),
          Ast.Num(9d)
        )),
        "y" → Ast.Str("vodka"))
      )
      assert(write(MyCaseClass3(("bear", 9d), "vodka")) == pattern)
    }

    /*{
      val source = Ast.Obj(Map(
        "x" → Ast.Arr(Seq(
          Ast.Str("bear"),
          Ast.Num("9.0")
        )),
        "y" → Ast.Str("vodka"))
      )
      val pattern = MyCaseClass3(("bear", 9d), "vodka")
      assert(read[MyCaseClass3](source) == pattern)
    }

    {
      val source = Ast.Obj(Map("x" → Ast.Num(1)))
      val pattern = WithDefaultParams(1, 100, Some(Point(1, 1)))
      assert(read[WithDefaultParams](source) == pattern)
    }

    {
      val invalidAst = Ast.False
      val exception = Try { read[WithDefaultParams](invalidAst) }
      // exception.message should be(s"Error while reading AST $invalidAst to WithDefaultParams")
    }

    {
      val pattern = Ast.Obj(Map("@theX" → Ast.Num(1), "y" → Ast.Num(2)))
      assert(write(WithKeyAnnotation(1, 2)) == pattern)
    }

    {
      val source = Ast.Obj(Map("@theX" → Ast.Num(1), "y" → Ast.Num(2)))
      val pattern = WithKeyAnnotation(1, 2)
      assert(read[WithKeyAnnotation](source) == pattern)
    }

    {
      val invalidAst = Ast.True
      val exception = Try { read[WithKeyAnnotation](invalidAst) }
      // exception.message should be(s"Error while reading AST $invalidAst to WithKeyAnnotation")
    }*/

    {
      val source = AppleReceipt("hello")
      val pattern = Ast("receipt-data" -> "hello")
      assert(write(source) == pattern)
    }

    /*{
      val pattern = AppleReceipt("hello")
      val source = Ast("receipt-data" -> "hello")
      assert(read[AppleReceipt](source) == pattern)
    }*/
  }  
}
