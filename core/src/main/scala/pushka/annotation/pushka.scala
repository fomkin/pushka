package pushka.annotation

import scala.annotation.{StaticAnnotation, tailrec}
import scala.language.experimental.macros
import scala.language.postfixOps
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

    val elseThrowPushkaException = cq"_ => throw pushka.PushkaException()"
    
    def checkValDefIsOption(x: ValDef) = {
      x.tpt.children.headOption match {
        case Some(tpeIdent: Ident) ⇒
          tpeIdent.name == TypeName("Option")
        case None ⇒ false
      }
    }

    def caseClassReader(className: TypeName, fields: List[ValDef]) = fields match {
      case field :: Nil ⇒
        q"${className.toTermName}(pushka.read[${field.tpt}](value))"
      case _ ⇒
        val fReaders = fields map { x ⇒
          if (checkValDefIsOption(x)) {
            q"${x.name} = m.get(${x.name.toString}).flatMap(x => pushka.read[${x.tpt}](x))"
          } else {
            q"${x.name} = pushka.read[${x.tpt}](m(${x.name.toString}))"
          }
        }
        q"""
          value match {
            case pushka.Ast.Obj(m) => ${className.toTermName}(..$fReaders)
            case _ ⇒ throw pushka.PushkaException()
          }
        """
    }

    def caseClassWriter(className: TypeName, fields: List[ValDef]) = fields match {
      case field :: Nil ⇒
        q"pushka.write(value.${field.name})"
      case _ ⇒
        def basicW(x: ValDef) = q"${x.name.toString} -> pushka.write(value.${x.name})"
        val (nonOpts, opts) = fields.partition(!checkValDefIsOption(_))
        val nonOptsWriters = nonOpts.map(x ⇒ q"b.append(${basicW(x)})")
        val optsWriters = opts map { x ⇒
          q"""
             if (config.leanOptions) {
               if (value.${x.name}.nonEmpty) b.append(${basicW(x)})
             } else {
               b.append(${basicW(x)})
             }
          """
        }
        q"""
          val b = scala.collection.mutable.Buffer.empty[(String, pushka.Ast)]
          ..$optsWriters
          ..$nonOptsWriters
          pushka.Ast.Obj(b.toMap)
        """
    }

    def caseClassRW(className: TypeName, typeParams: List[TypeDef], fields: List[ValDef]) = {
      val reader = caseClassReader(className, fields)
      val writer = caseClassWriter(className, fields)
      
      typeParams match {
        case Nil ⇒
          q"""
            implicit def _rw(implicit config: pushka.Config = pushka.Config.default): pushka.RW[$className] = new pushka.RW[$className] {
              def read(value: pushka.Ast) = $reader
              def write(value: $className): pushka.Ast = $writer
            }
          """
        case _ ⇒
          @tailrec
          def makeRWsRec(i: Int, acc: List[Tree], tl: List[TypeName]): List[Tree] = tl match {
            case x :: xs ⇒
              val (ri, wi) = (TermName("r" + i), TermName("w" + i))
              val r = q"$ri : pushka.Reader[$x]"
              val w = q"$wi : pushka.Writer[$x]"
              makeRWsRec(i + 1, r :: w :: acc, xs)
            case Nil ⇒ acc
          }
          val defs = typeParams.map(x ⇒ TypeDef(Modifiers(), TypeName(x.name.toString), Nil, x.rhs))
          val params = defs.map(_.name)
          val rws = makeRWsRec(0, Nil, params)
          q"""
            implicit def _rw[..$defs](implicit ..$rws, config: pushka.Config = pushka.Config.default): pushka.RW[$className[..$params]] = {
              new pushka.RW[$className[..$params]] {
                def read(value: pushka.Ast) = $reader
                def write(value: $className[..$params]): pushka.Ast = $writer
              }
            }
          """
      }
    }

    def sealedTraitRW(traitName: TypeName, compDecl: ModuleDef) = {
      // Left: name of case object
      // Right: name of case class 
      type VariantName = Either[TermName, TypeName]
      
      // Update body so that cases classes contained in companion object
      // of a sealed trait will processed by Pushka (i.e. generate RWs for them).
      @tailrec def genUpdatedBody(acc: List[Tree], tail: List[Tree]): List[Tree] = tail match {
        case Nil ⇒ acc
        case x :: xs ⇒ x match {
          case classDecl @ q"$mods class $n(..$fields) extends ..$p" if mods.hasFlag(Flag.CASE) && checkBases(p) ⇒
            val newAcc = classDecl :: modifiedCompanion(None, caseClassRW(n, Nil, fields), n) :: acc
            genUpdatedBody(newAcc, xs)
          case ignore ⇒ genUpdatedBody(ignore :: acc, xs)
        }
      }
      
      // Check base classes contains this sealed trait.  
      def checkBases(xs: List[Tree]) = xs exists {
        case x: Ident ⇒ x.name == traitName
        case _ ⇒ false
      }
      
      // Convert scala names to JSON object key names
      // MyCaseObject -> myCaseObject
      def variantName(n: Name) = {
        val s = n.toString
        s.charAt(0).toLower + s.substring(1)
      }
      
      def genValueMatching(names: Seq[VariantName])(genCase: VariantName ⇒ Tree): Tree = {
        q"""
          value match {
            case ..${names.map(genCase) :+ elseThrowPushkaException}
          }
        """
      }

      val q"object $obj extends ..$bases { ..$body }" = compDecl
      val updatedBody = genUpdatedBody(Nil, body)
      
      val names: Seq[VariantName] = {
        val list = (body: List[Tree]) collect {
          case q"$mods object $n extends ..$p { ..$body }"
            if checkBases(p) && mods.hasFlag(Flag.CASE) ⇒ Left(n)
          case q"$mods class $n(..$fields) extends ..$p  { ..$body }"
            if checkBases(p) && mods.hasFlag(Flag.CASE) ⇒ Right(n)
        }                                            
        // Case classes are matched by variable pattern (see bellow)
        // it disallow to match anything elese (SLS 8.1.1).
        // So we need to sort this list so that case objects stands in beginning
        list.sortBy(_.isRight).toSeq
      }
      
      // Matching on pushka.Ast to find right reader
      val readMatcher = genValueMatching(names) {
        case Right(n) ⇒
          val lower = variantName(n)
          cq"pushka.Ast.Obj(m) if m.contains($lower) => ${n.toTermName}._rw.read(m($lower))"
        case Left(n) ⇒ cq"pushka.Ast.Str(${variantName(n)}) => $n"
      }
      
      // Matching on pushka.Ast to find right writer
      val writeMatcher = genValueMatching(names) {
        case Right(n) ⇒
          val lower = variantName(n)
          cq"o: $n => pushka.Ast.Obj(Map($lower -> ${n.toTermName}._rw.write(o)))"
        case Left(n) ⇒ cq"`$n` => pushka.Ast.Str(${variantName(n)})"
      }

      q"""
        object ${traitName.toTermName} { 
          ..$updatedBody
          implicit val _rw: pushka.RW[$traitName] = new pushka.RW[$traitName] {
            def read(value: pushka.Ast) = $readMatcher
            def write(value: $traitName): pushka.Ast = $writeMatcher
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
        case ClassDef(mods, traitName, _, _) if mods.hasFlag(Flag.SEALED) && mods.hasFlag(Flag.TRAIT) ⇒
          compDeclOpt match {
            case Some(x) ⇒ sealedTraitRW(traitName, x)
            case None ⇒ c.abort(c.enclosingPosition, "Companion object declaration expected")
          }
        case _ =>
          println("Error: "+showRaw(classDecl))
          c.abort(classDecl.pos,
            "Pushka works only with case classes (including generic) " +
            "and sealed traits (not generic)"
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
