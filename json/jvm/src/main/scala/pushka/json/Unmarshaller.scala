package pushka.json

import pushka.Value
import jawn.Parser

final class Unmarshaller(val self: String) extends AnyVal {
  def parseJSON: Value = Parser.parseFromString(self)(PushkaFacade).get
}
