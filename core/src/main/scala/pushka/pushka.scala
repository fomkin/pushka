package pushka

import scala.annotation.{tailrec, StaticAnnotation}
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

class pushka extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro pushkaMacro.impl
}

/**
 * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
 */
object pushkaMacro {

  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    
    import c.universe._

    def checkValDefIsOption(x: ValDef) = {
      x.tpt.children.headOption match {
        case Some(tpeIdent: Ident) ⇒
          tpeIdent.name == TypeName("Option")
        case None ⇒ false
      }
    }

    def readerN(n: Int) = TermName(s"r$n")

    def writerN(n: Int) = TermName(s"w$n")

    def caseClassReader(className: TypeName, fields: List[ValDef]) = fields match {
      case field :: Nil ⇒
        q"${className.toTermName}(${readerN(0)}.read(value))"
      case _ ⇒
        val withIndex = fields.zipWithIndex
        val fReaders = withIndex map {
          case (x, i) if checkValDefIsOption(x) ⇒
            q"${x.name} = m.get(${x.name.toString}).flatMap(x => ${readerN(i)}.read(x))"
          case (x, i) ⇒
            q"${x.name} = ${readerN(i)}.read(m(${x.name.toString}))"
        }
        q"""
          value match {
            case pushka.Value.Obj(m) => ${className.toTermName}(..$fReaders)
            case _ ⇒ throw pushka.PushkaException()
          }
        """
    }

    def caseClassWriter(className: TypeName, fields: List[ValDef]) = fields match {
      case field :: Nil ⇒
        q"${writerN(0)}.write(value.${field.name})"
      case _ ⇒
        def basicW(x: ValDef, i: Int) = {
          q"${x.name.toString} -> ${writerN(i)}.write(value.${x.name})"
        }
        val (nonOpts, opts) = fields.zipWithIndex partition {
          case (x, i) if checkValDefIsOption(x) ⇒ false
          case (x, i) ⇒ true
        }
        val nonOptsWriters = nonOpts.map { case (x, i) ⇒ basicW(x, i) }
        val optsWriters = opts map {
          case (x, i) ⇒ q"if (value.${x.name}.isEmpty) None else Some(${basicW(x, i)})"
        }
        q"""
          val opts = Seq(..$optsWriters).flatten
          val xs = Seq(..$nonOptsWriters) ++ opts
          pushka.Value.Obj(Map(xs:_*))
        """
    }

    def caseClassRW(className: TypeName, typeParams: List[TypeDef], fields: List[ValDef]) = {
      @tailrec
      def makeRWsRec(i: Int, acc: List[Tree], tl: List[ValDef]): List[Tree] = tl match {
        case x :: xs ⇒
          val r = q"${readerN(i)}: pushka.Reader[${x.tpt}]"
          val w = q"${writerN(i)}: pushka.Writer[${x.tpt}]"
          makeRWsRec(i+1, r :: w :: acc, xs)
        case Nil ⇒ acc
      }
      val rws = makeRWsRec(0, Nil, fields)
      val reader = caseClassReader(className, fields)
      val writer = caseClassWriter(className, fields)
      
      typeParams match {
        case Nil ⇒
          q"""
            implicit def _rw(implicit ..$rws): pushka.RW[$className] = new pushka.RW[$className] {
              def read(value: pushka.Value) = $reader
              def write(value: $className): pushka.Value = $writer
            }
          """
        case xs ⇒
          
          val tDefs = xs.map(x ⇒ TypeDef(Modifiers(), TypeName(x.name.toString), Nil, x.rhs))
          val tParams = tDefs.map(_.name)
          
          q"""
            implicit def _rw[..$tDefs](implicit ..$rws): pushka.RW[$className[..$tParams]] = new pushka.RW[$className[..$tParams]] {
              def read(value: pushka.Value) = $reader
              def write(value: $className[..$tParams]): pushka.Value = $writer
            }
          """
      }
    }

    def sealedTraitRW(traitName: TypeName, compDecl: ModuleDef) = {
      
      @tailrec def updateBodyRec(acc: List[Tree], tail: List[Tree]): List[Tree] = tail match {
        case Nil ⇒ acc
        case x :: xs ⇒ x match {
          case classDecl @ q"case class $n(..$fields) extends ..$p" if checkBases(p) ⇒
            val newAcc = classDecl :: modifiedCompanion(None, caseClassRW(n, Nil, fields), n) :: acc
            updateBodyRec(newAcc, xs)
          case ignore ⇒ updateBodyRec(ignore :: acc, xs)
        }
      }
      
      def checkBases(xs: List[Tree]) = {
        xs.asInstanceOf[List[Ident]].exists(_.name == traitName)
      }
      
      def jsonVariantConvention(n: Name) = {
        val s = n.toString
        s.charAt(0).toLower + s.substring(1)
      }
      
      val q"object $obj extends ..$bases { ..$body }" = compDecl
      val updatedBody = updateBodyRec(Nil, body)
      val names: List[Either[TermName, TypeName]] = (body: List[Tree]) collect {
        case q"case object $n extends ..$p" if checkBases(p) ⇒ Left(n)
        case q"case class $n(..$fields) extends ..$p" if checkBases(p) ⇒ Right(n)
      }
      
      // Matching on pushka.Value to find right
      // reader
      // TODO Avoid match errors
      val readMatcher = Match(Ident(TermName("value")), names map {
        case Left(n) ⇒
          CaseDef(q"pushka.Value.Str(${jsonVariantConvention(n)})", q"$n")
        case Right(n) ⇒
          val lower = jsonVariantConvention(n)
          CaseDef(
            Bind(
              TermName("o"),
              Typed(
                Ident(termNames.WILDCARD),
                TypeTree(typeOf[pushka.Value.Obj])
              )
            ),
            q"o.value.contains($lower)",
            q"${n.toTermName}._rw.read(o.value($lower))"
          )
      })
      
      // Matching on pushka.Value to find right
      // writer
      // TODO Avoid match errors
      val writeMatcher = Match(Ident(TermName("value")), names map {
        case Left(n) ⇒
          CaseDef(q"$n", q"pushka.Value.Str(${jsonVariantConvention(n)})")
        case Right(n) ⇒
          val lower = jsonVariantConvention(n)
          CaseDef(
            Bind(TermName("o"), Typed(Ident(termNames.WILDCARD), Ident(n))),
            q"pushka.Value.Obj(Map($lower -> ${n.toTermName}._rw.write(o)))"
          )
      })

      q"""
        object ${traitName.toTermName} { 
          ..$updatedBody
          implicit val _rw: RW[$traitName] = new RW[$traitName] {
            def read(value: pushka.Value) = $readMatcher
            def write(value: $traitName): pushka.Value = $writeMatcher
          }
        }
       """
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
