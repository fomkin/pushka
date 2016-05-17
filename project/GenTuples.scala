import sbt.File
import sbt._

object GenTuples extends (File ⇒ Seq[File]) {
  def apply(dir: File): Seq[File] = {
    val file = dir / "pushka" / "Generated.scala"
    val defs = 2 to 22 map { n ⇒
      val range = 1 to n
      val typeArgs = range.map("T"+_.toString).toSeq
      val typeArgsJ = typeArgs.mkString(", ")
      val rws = range.map(n ⇒ s"rw$n: RW[${typeArgs(n-1)}]").mkString(", ")
      val readers = range.map(n ⇒ s"rw$n.read(xsi.next())")
      val writers = range.map(n ⇒ s"rw$n.write(value._$n)")
      s"""  implicit def tuple$n[$typeArgsJ](implicit $rws): RW[($typeArgsJ)] = new RW[($typeArgsJ)] {
         |    def read(value: Ast): ($typeArgsJ) = value match {
         |      case Ast.Arr(xs) ⇒
         |        val xsi = xs.iterator
         |        (${readers.mkString(", ")})
         |      case _ ⇒ throw PushkaException(value, "Tuple")
         |    }
         |    def write(value: ($typeArgsJ)): Ast = {
         |      Ast.Arr(Vector(${writers.mkString(", ")}))
         |    }
         |  }
       """.stripMargin
    }
    IO.write(file,
      s"""
         |package pushka
         |
         |// This file was generated
         |trait Generated {
         |${defs.mkString("\n")}
         |}
      """.stripMargin
    )
    Seq(file)
  }
}
