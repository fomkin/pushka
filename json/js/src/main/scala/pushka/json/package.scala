package pushka

import scala.language.implicitConversions

package object json {

  implicit def toMarshaller(value: Value): Marshaller = new Marshaller(value)
  implicit def toUnmarshaller(value: String): Unmarshaller = new Unmarshaller(value)
}
