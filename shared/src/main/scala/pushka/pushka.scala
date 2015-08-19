package pushka

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
 * Thanks to https://github.com/kifi/json-annotation/
 */
class pushka extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro pushkaMacro.impl
}

/**
 * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
 */
object pushkaMacro {

  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def caseClassRW(className: TypeName, typeParams: List[TypeDef], fields: List[ValDef]) = {
      def rwN(n: Int) = TermName(s"rw$n")
      val withIndex = fields.zipWithIndex
      val fRWs = withIndex map { case (x, i) ⇒ q"${rwN(i)}: pushka.RW[${x.tpt}]" }
      val fWriters = withIndex map {
        case (x, i) ⇒ q"${x.name.toString} -> ${rwN(i)}.write(value.${x.name})"
      }
      val fReaders = withIndex map {
        case (x, i) ⇒ q"${x.name} = ${rwN(i)}.read(m(${x.name.toString}))"
      }
      typeParams match {
        case Nil ⇒
          q"""
            implicit def _rw(implicit ..$fRWs): pushka.RW[$className] = new pushka.RW[$className] {
              def read(value: pushka.Value) = value match {
                case pushka.Value.Obj(m) => ${className.toTermName}(..$fReaders)
                case _ ⇒ throw pushka.PushkaException()
              }
              def write(value: $className): pushka.Value = {
                pushka.Value.Obj(Map(..$fWriters))
              }
            }
          """
        case xs ⇒
          val tDefs = xs.map(x ⇒ TypeDef(Modifiers(), TypeName(x.name.toString), Nil, x.rhs))
          val tParams = tDefs.map(_.name)
          q"""
            implicit def _rw[..$tDefs](implicit ..$fRWs): pushka.RW[$className[..$tParams]] = new pushka.RW[$className[..$tParams]] {
              def read(value: pushka.Value) = value match {
                case pushka.Value.Obj(m) => ${className.toTermName}(..$fReaders)
                case _ ⇒ throw pushka.PushkaException()
              }
              def write(value: $className[..$tParams]): pushka.Value = {
                pushka.Value.Obj(Map(..$fWriters))
              }
            }
          """
      }
    }

    def sealedTraitRW(traitName: TypeName, compDecl: ModuleDef) = {
      val q"object $obj extends ..$bases { ..$body }" = compDecl
      body map {
//        case q"case object $caseObjName extends $traitName" ⇒
//          println("case object " + caseObjName.toString)
        case caseClassDecl @ q"case class $className(..$fields) extends $traitName" ⇒
          println("!!!!!!!!!!!!!!!")
          val rw = caseClassRW(className, Nil, fields)
          q"""
             $caseClassDecl
             ${modifiedCompanion(None, rw, className)}
          """
        case member ⇒ member
        //        case xx ⇒ println(xx)
      }
      val xx =
      q"""
        object ${traitName.toTermName} { ..$body }
       """
      println(show(xx))
      xx
    }
    
    def modifiedCompanion(compDeclOpt: Option[ModuleDef], rw: Tree, className: TypeName) = {
      compDeclOpt map { compDecl =>
        // Add the formatter to the existing companion object
        val q"object $obj extends ..$bases { ..$body }" = compDecl
        q"""
          object $obj extends ..$bases {
            ..$body
            $rw
          }
        """
      } getOrElse {
        q"object ${className.toTermName} { $rw }"
      }
    }

    def modifiedDeclaration(classDecl: ClassDef, compDeclOpt: Option[ModuleDef] = None) = {
      val compDecl = classDecl match {
        case q"case class $className[..$typeParams](..$fields) extends ..$bases { ..$body }" ⇒
          val rw = caseClassRW(className, typeParams, fields)
          modifiedCompanion(compDeclOpt, rw, className)
        case q"case class $className(..$fields) extends ..$bases { ..$body }" ⇒
          val rw = caseClassRW(className, Nil, fields)
          modifiedCompanion(compDeclOpt, rw, className)
        case q"sealed abstract trait $traitName extends ..$bases" ⇒
          compDeclOpt match {
            case Some(x) ⇒ sealedTraitRW(traitName, x)
            case None ⇒ c.abort(c.enclosingPosition, "Companion object declaration expected")
          }
        case _ =>
          c.abort(c.enclosingPosition,
            "Annotation is only supported on case classes " +
            "and sealed traits (without type parameters)"
          )
      }
      // Return both the class and companion object declarations
      c.Expr(q"""
        $classDecl
        $compDecl
      """)
    }

    annottees.map(_.tree) match {
      case (classDecl: ClassDef) :: Nil => modifiedDeclaration(classDecl)
      case (classDecl: ClassDef) :: (compDecl: ModuleDef) :: Nil => modifiedDeclaration(classDecl, Some(compDecl))
      case _ => c.abort(c.enclosingPosition, "Invalid annottee")
    }
  }
}

