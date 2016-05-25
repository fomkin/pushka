package pushka

import java.util.UUID

trait DefaultObjectKeys {

  implicit val intOk = new ObjectKey[Int] {
    def stringify(x: Int): String = x.toString
    def parse(x: String): Int = x.toInt
  }

  implicit val stringToOK = new ObjectKey[String] {
    def stringify(x: String): String = x
    def parse(x: String): String = x
  }

  implicit val uuidToOK = new ObjectKey[UUID] {
    def stringify(x: UUID): String = x.toString
    def parse(x: String): UUID = UUID.fromString(x)
  }

}
