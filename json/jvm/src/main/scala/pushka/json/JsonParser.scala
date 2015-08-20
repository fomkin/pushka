package pushka.json

import pushka.Ast

final class JsonParser extends pushka.Parser[String] {
  def parse(data: String): Ast = {
    jawn.Parser.parseFromString(data)(PushkaFacade).get
  }
}
