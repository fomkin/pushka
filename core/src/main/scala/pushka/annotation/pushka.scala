package pushka.annotation

import scala.language.experimental.macros
import scala.language.postfixOps

import scala.annotation.{StaticAnnotation, tailrec}
import scala.reflect.macros.blackbox

import macrocompat.bundle

class pushka extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any =
    macro PushkaAnnotatioMacro.impl
}

/**
 * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
 */
@bundle class PushkaAnnotatioMacro(val c: blackbox.Context) {

  import c.universe._

  def impl(annottees: c.Expr[Any]*): c.Expr[Any] = {

    def findAnnotationFlag(name: String, annotations: List[c.Tree]): Boolean = {
      annotations exists {
        case q"new $name()" ⇒ true
        case _ => false
      }
    }
    def customKeyForFiled(field: ValDef): Option[String] = {
      field.mods.annotations collectFirst {
        case q"new key(${s: String})" ⇒ s
      }
    }

    def keyFromField(field: ValDef): String = {
      customKeyForFiled(field).fold(field.name.toString)(identity)
    }

    def caseClassReader(className: TypeName, fields: List[ValDef], annotations: List[c.Tree]) = fields match {
      case field :: Nil if !findAnnotationFlag("forceObject", annotations) ⇒
        if (customKeyForFiled(field).nonEmpty) {
          c.warning(field.pos,
            "@key annotation will ignored cause classes with one filed writes without object " +
            "wrapper. Use @forceObject annotation to avoid with behavior."
          )
        }
        q"${className.toTermName}(pushka.read[${field.tpt}](value))"
      case _ ⇒
        val fReaders = fields map { x ⇒
          if (x.rhs.nonEmpty) {
            q"""
              ${x.name} = m.get(${keyFromField(x)}).
                map(x => pushka.read[${x.tpt}](x)).
                getOrElse(${x.rhs})
            """
          } else {
            q"""
              ${x.name} = if (m.contains(${keyFromField(x)})) {
                pushka.read[${x.tpt}](m(${keyFromField(x)}))
              } else {
                pushka.HasDefault[${x.tpt}] match {
                  case None =>
                    throw new pushka.PushkaException(
                      ${className.toString} + " should contain " + ${keyFromField(x)})
                  case Some(default) => default.value
                }
              }
            """
          }
        }
        q"""
          value match {
            case pushka.Ast.Obj(m) => ${className.toTermName}(..$fReaders)
            case _ ⇒ throw pushka.PushkaException(value, ${className.toTermName.toString})
          }
        """
    }

    def caseClassWriter(className: TypeName, fields: List[ValDef], annotations: List[c.Tree]) = fields match {
      case field :: Nil if !findAnnotationFlag("forceObject", annotations) ⇒
        q"pushka.write(value.${field.name})"
      case _ ⇒
        def basicW(field: ValDef) = {
          val key = keyFromField(field)
          q"$key -> pushka.write(value.${field.name})"
        }
        val writers = fields map { field ⇒
          val t = field.tpt
          val n = field.name
          val w = basicW(field)
          q"""
            pushka.HasDefault[$t] match {
              case Some(default) if !default.writeDefault && value.$n == default.value =>
                // do nothing
              case _ => b.append($w)
            }
          """
        }
        q"""
          val b = scala.collection.mutable.Buffer.empty[(String, pushka.Ast)]
          ..$writers
          pushka.Ast.Obj(b.toMap)
        """
    }

    def caseClassRW(className: TypeName, typeParams: List[TypeDef], fields: List[ValDef], annotations: List[c.Tree]) = {
      val reader = caseClassReader(className, fields, annotations)
      val writer = caseClassWriter(className, fields, annotations)

      typeParams match {
        case Nil ⇒
          q"""
            implicit val _rw: pushka.RW[$className] = new pushka.RW[$className] {
              def read(value: pushka.Ast) = $reader
              def write(value: $className): pushka.Ast = $writer
            }
          """
        case _ ⇒
          @tailrec
          def makeRWsRec(i: Int, acc: List[Tree], tl: List[TypeName]): List[Tree] = tl match {
            case x :: xs ⇒
              val (ri, wi, di) = (TermName("r" + i), TermName("w" + i), TermName("d" + i))
              val r = q"$ri : pushka.Reader[$x]"
              val w = q"$wi : pushka.Writer[$x]"
              val d = q"$di : pushka.HasDefault[$x]"
              makeRWsRec(i + 1, d :: r :: w :: acc, xs)
            case Nil ⇒ acc
          }
          val defs = typeParams.map(x ⇒ TypeDef(Modifiers(), TypeName(x.name.toString), Nil, x.rhs))
          val params = defs.map(_.name)
          val rwds = makeRWsRec(0, Nil, params)
          q"""
            implicit def _rw[..$defs](implicit ..$rwds): pushka.RW[$className[..$params]] = {
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
      @tailrec def genUpdatedBody(acc: List[Tree], tail: List[Tree]): List[Tree] = {
        def checkCaseClass(classDecl: ClassDef, compDecl: Option[ModuleDef]) = classDecl match {
          case q"$mods class $n(..$fields) extends ..$p { ..$body }" if mods.hasFlag(Flag.CASE) && checkSuperClass(p) ⇒
            classDecl :: modifiedCompanion(compDecl, caseClassRW(n, Nil, fields, mods.annotations), n) :: acc
          case _ ⇒ classDecl :: acc
        }
        def compareNames(classDecl: ClassDef, compDecl: ModuleDef): Boolean = {
          classDecl.name.toString == compDecl.name.toString
        }
        tail match {
          case Nil ⇒ acc
          case (classDecl: ClassDef) :: (compDecl: ModuleDef) :: xs if compareNames(classDecl, compDecl) ⇒
            val newAcc = checkCaseClass(classDecl, Some(compDecl))
            genUpdatedBody(newAcc, xs)
          case (classDecl: ClassDef) :: xs ⇒
            val newAcc = checkCaseClass(classDecl, None)
            genUpdatedBody(newAcc, xs)
          case ignore :: xs ⇒ genUpdatedBody(ignore :: acc, xs)
        }
      }

      // Check base classes contains this sealed trait.
      def checkSuperClass(xs: List[Tree]) = xs exists {
        case x: Ident ⇒ x.name == traitName
        case _ ⇒ false
      }

      // Convert scala names to JSON object key names
      // MyCaseObject -> myCaseObject
      def variantName(n: Name) = {
        val s = n.toString
        s.charAt(0).toLower + s.substring(1)
      }

      def genValueMatching(names: Seq[VariantName], read: Boolean)(genCase: VariantName ⇒ Tree): Tree = {
        val elseThrowPushkaException = if (read) {
          cq"_ => throw pushka.PushkaException(value, ${traitName.toTermName.toString})"
        } else {
          cq"_ => throw pushka.PushkaException(value)"
        }

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
            if checkSuperClass(p) && mods.hasFlag(Flag.CASE) ⇒ Left(n)
          case q"$mods class $n(..$fields) extends ..$p  { ..$body }"
            if checkSuperClass(p) && mods.hasFlag(Flag.CASE) ⇒ Right(n)
        }
        // Case classes are matched by variable pattern (see bellow)
        // it disallow to match anything elese (SLS 8.1.1).
        // So we need to sort this list so that case objects stands in beginning
        list.sortBy(_.isRight)
      }

      // Matching on pushka.Ast to find right reader
      val readMatcher = genValueMatching(names, read = true) {
        case Right(n) ⇒
          val lower = variantName(n)
          cq"pushka.Ast.Obj(m) if m.contains($lower) => ${n.toTermName}._rw.read(m($lower))"
        case Left(n) ⇒ cq"pushka.Ast.Str(${variantName(n)}) => $n"
      }

      // Matching on pushka.Ast to find right writer
      val writeMatcher = genValueMatching(names, read = false) {
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
        case q"$mods class $className[..$typeParams](..$fields) extends ..$bases { ..$body }" if mods.hasFlag(Flag.CASE) ⇒
          val rw = caseClassRW(className, typeParams, fields, mods.annotations)
          modifiedCompanion(compDeclOpt, rw, className)
        case q"$mods class $className(..$fields) extends ..$bases { ..$body }" if mods.hasFlag(Flag.CASE) ⇒
          val rw = caseClassRW(className, Nil, fields, mods.annotations)
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
