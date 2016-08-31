package pushka.json

import pushka.{Ast, Printer}

import scala.annotation.switch

/**
 * Port of Upickle printer
 * https://github.com/lihaoyi/upickle-pprint/blob/master/upickle/jvm/src/main/scala/upickle/json/Jawn.scala
 */
final class JsonPrinter extends Printer[String] {
  
  def print(jv: Ast): String = {
    val sb = new StringBuilder
    render(sb, 0, jv)
    sb.toString()
  }

  def render(sb: StringBuilder, depth: Int, jv: Ast): Unit =
    jv match {
      case Ast.Null => sb.append("null")
      case Ast.True => sb.append("true")
      case Ast.False => sb.append("false")
      case Ast.Num(n) => sb.append(n)
      case Ast.Str(s) => renderString(sb, s)
      case Ast.Arr(xs) => renderArray(sb, depth, xs)
      case Ast.Obj(vs) => renderObject(sb, depth, canonicalizeObject(vs))
    }

  def canonicalizeObject(vs: Map[String, Ast]): Iterator[(String, Ast)] = {
    vs.iterator
  }

  def renderString(sb: StringBuilder, s: String): Unit = {
    escape(sb, s, unicode = false)
  }

  def renderArray(sb: StringBuilder, depth: Int, vs: Iterable[Ast]): Unit = {
    if (vs.isEmpty) sb.append("[]")
    else {
      sb.append("[")
      val iter = vs.iterator
      render(sb, depth + 1, iter.next())
      while (iter.hasNext) {
        sb.append(",")
        render(sb, depth + 1, iter.next())
      }
      sb.append("]")
    }
  }

  def renderObject(sb: StringBuilder, depth: Int, it: Iterator[(String, Ast)]): Unit = {
    if (!it.hasNext) { sb.append("{}"); () } else {
      val (k0, v0) = it.next()
      sb.append("{")
      renderString(sb, k0)
      sb.append(":")
      render(sb, depth + 1, v0)
      while (it.hasNext) {
        val (k, v) = it.next()
        sb.append(",")
        renderString(sb, k)
        sb.append(":")
        render(sb, depth + 1, v)
      }
      sb.append("}")
    }
  }

  def escape(sb: StringBuilder, s: String, unicode: Boolean): Unit = {
    sb.append('"')
    var i = 0
    val len = s.length
    while (i < len) {
      (s.charAt(i): @switch) match {
        case '"' => sb.append("\\\"")
        case '\\' => sb.append("\\\\")
        case '\b' => sb.append("\\b")
        case '\f' => sb.append("\\f")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\t' => sb.append("\\t")
        case c =>
          if (c < ' ' || (c > '~' && unicode)) sb.append("\\u%04x" format c.toInt)
          else sb.append(c)
      }
      i += 1
    }
    sb.append('"')
  }
}
