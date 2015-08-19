package pushka

case class PushkaException(message: Option[String] = None)
  extends Exception(message.fold("Unexpected data")(identity))
