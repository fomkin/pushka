package pushka

import scala.language.implicitConversions

package object json extends PushkaJsonBackend {
  implicit val printer = new JsonPrinter()
  implicit val parser = new JsonParser()
}
