# Pushka

[![Build Status](https://travis-ci.org/fomkin/pushka.svg?branch=develop)](https://travis-ci.org/fomkin/pushka)

Pushka is a pickler implemented without any runtime reflection. 
We want to achieve high stability of pickling of
complex structures and give pretty output without 
service keywords and wrappers.   

  * Both Scala and Scala.js
  * Sealed traits and case classes pickling 
  * Most of standard scala data types pickling 
  * Really human-readable output (see bellow)
  * Not only JSON. You can implement your own backend

# Getting Started

Configure SBT file

```scala
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)

// For Scala.js
libraryDependencies += "com.github.fomkin" %%% "pushka-json" % "0.1.0"

// For Scala.jvm
libraryDependencies += "com.github.fomkin" %% "pushka-json" % "0.1.0"
```

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

// Companion object is required for sealed traits.
object User {
  // Case classes and case object defined within
  // companion object with be used for sealed trait pickling.
  case object Empty extends User
  // BTW you can use this case classes alone (not case objects)
  case class Name(first: String, last: String) extends User
  case class Password(value: String) extends User
}
```

Use it

```scala
import pushka.json._

write(Structure("a", Some("b"))) // { "field1": "a", "field2": "b" }
write(SimpleStructure(42)) // 42

write[User](User.Empty) // "empty"
write[User](User.Name("John", "Doe")) // { "name": { "first": "John", "last": "Doe" } }
write[User](User.Password("boobs")) // { "password": "boobs" }
```

```scala
import pushka.json._

read[Structure](""" { "field1": "a" } """) //  Structure("a", None)
read[SimpleStructure](42) // SimpleStructure(42)

read[User](""" "empty" """) // User.Empty
read[User](""" { "name": { "first": "John", "last": "Doe" } } """) // User.Name("John", "Doe")
read[User](""" { "password": "boobs" } """) // User.Password("boobs")
```
