package pushka.json

import pushka._

trait PushkaJsonBackend extends DefaultRWs with DefaultObjectKeys {

  def write[T](value: T)(implicit writer: Writer[T], printer: Printer[String]): String = {
    printer.print(writer.write(value))
  }

  def read[T](value: String)(implicit reader: Reader[T], parser: Parser[String]): T = {
    reader.read(parser.parse(value))
  }
}
