import org.scalatest._
import pushka._

object SealedTraitSpec {

  @pushka sealed trait User

  object User {
    case object Empty extends User
    case class Name(first: String, last: String) extends User
  }

  @pushka case class Container(user: User, anotherField: Int)
}

class SealedTraitSpec extends FlatSpec with Matchers {

  import SealedTraitSpec._

  "Variant based on case class" should "writes by case class rules in property" in {
    val instance = Container(User.Name("John", "Doe"), 10)
    write(instance) should be(
      Value.Obj(Map(
        "user" → Value.Obj(Map(
          "name" → Value.Obj(Map(
            "first" → Value.Str("John"),
            "last" → Value.Str("Doe"))
          ))),
        "anotherField" → Value.Number(10.0)
      ))
    )
  }

  "Variant based on case object" should "writes as simple string" in {
    val instance = Container(User.Empty, 10)
    write(instance) should be(
      Value.Obj(Map(
        "user" → Value.Str("empty"),
        "anotherField" → Value.Number(10.0))
      )
    )
  }
}
