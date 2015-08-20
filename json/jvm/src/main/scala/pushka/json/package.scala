package pushka

package object json extends PushkaJsonBackend {

  implicit val printer = new JsonPrinter()
  implicit val parser = new JsonParser()
}
