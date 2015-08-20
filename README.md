### Pushka

Pushka is a pure Scala annotation based pickler implemented with macros without runtime reflection.

  * Scala 2.11 and Scala.js support
  * Human-readable JSON printer (see bellow)
  * Not only JSON. You can implement your own backend

### Getting Started

Pushka now in early development stage. So only local snapshots are available.

Add the following to your SBT config:

    libraryDependencies ++= Seq(
      "com.github.fomkin" %%% "pushka-core" % "0.1.0-SNAPSHOT"
      "com.github.fomkin" %%% "pushka-json" % "0.1.0-SNAPSHOT"
    )

Define case classes

```scala
import pushka.annotation._

@pushka case class Structure(field1: String, field2: Option[String])

@pushka case class SimpleStructure(x: Int)
```

Define ADTs

```scala
import pushka.annotation._

@pushka sealed trait User

object User {
  case object Empty extends User
  case class Name(first: String, last: String) extends User
  case class Password(value: String) extends User
}
```

Use it

```scala
import pushka.json._

write(Structure("a", Some("b"))) // { "field1": "a", "field2": "b" }
write(SimpleStrcuture(42)) // 42

write[User](User.Empty) // "empty"
write[User](User.Name("John", "Doe")) // { "name": { "first": "John", "last": "Doe" } }
write[User](User.Password("boobs")) // { "password": "boobs" }
```

```scala
import pushka.json._

read[Structure](""" { "field1": "a" } """) //  Structure("a", None)
read[SimpleStrcuture](42) // SimpleStrcuture(42)

read[User](""" "empty" """) // User.Empty
read[User](""" { "name": { "first": "John", "last": "Doe" } } """) // User.Name("John", "Doe")
read[User](""" { "password": "boobs" } """) // User.Password("boobs")
```
