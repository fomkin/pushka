# Pushka

[![Build Status](https://travis-ci.org/fomkin/pushka.svg?branch=develop)](https://travis-ci.org/fomkin/pushka) [![Join the chat at https://gitter.im/fomkin/pushka](https://badges.gitter.im/fomkin/pushka.svg)](https://gitter.im/fomkin/pushka?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Pushka is a serialization library implemented without any runtime reflection. It created to reach well human readability of output JSON and good performance. Pushka works well both on Scala (2.10, 2.11) and Scala.js.

# Motivation

1. Most of the serialization libraries are writes case classes "as is". For example, if we have `Option` value it will be written with some kind of wrapper. In the case of sealed traits, most libraries write metadata: trait name and case class name. This makes JSON unreadable by human and makes it useless for creating public API. We want to achieve high human readability of output JSON: no wrappers if it possible, no metadata ever.

2. Codebase simplicity. In our work, we encountered that some libraries based on implicit macros (including shapeless based) are fails on our data. In this project, we want to make the code as simple as possible to find bugs faster.

3. High performance. Minimum runtime overhead. See [Boopickle benchmarks](http://ochrons.github.io/boopickle-perftest/).


# Usage

Add Pushka dependency to your project.

```scala
// For Scala.js
libraryDependencies += "com.github.fomkin" %%% "pushka-json" % "0.7.0"

// For Scala.jvm
libraryDependencies += "com.github.fomkin" %% "pushka-json" % "0.7.0"
```
Pushka uses macro annotations which implemented in macro paradise plugin. Unfortunately, it can't be added transitively by Pushka dependency, so you need to plug it manually.

```scala
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
```
Let define types we want to write to JSON.

```scala
import pushka.annotation._

@pushka case class User(
  email: String,
  name: Option[String],
  role: Role
)

@puska sealed trait Role

object Role {
  case object Moderator extends Role
  case object Accountant extends Role
  case class Group(xs: Seq[Role]) extends Role
}
```
Ok. Now let create data and write it to JSON

```scala
import pushka.json._

val data = User(
  email = "john@example.com",
  name = None,
  role = Role.Accountant
)

println(write(data))
```

```json
{
  "email": "john@example.com",
  "role": "accountant"
}
```

Ok. Change users role.

```scala
data.copy(role = Role.Group(Role.Accountant, Role.Moderator))
```
```json
{
  "email": "john@example.com",
  "role": {
    "group": ["accountant", "moderator"]
  }  
}
```
Add user name.
```scala
data.copy(name = Some("Jonh Doe"))
```
```json
{
  "email": "john@example.com",
  "name": "John Doe",
  "role": {
    "group": ["accountant", "moderator"]
  }  
}
```

Now, in the opposite direction. Let's read JSON.

```scala
val json = """
  {
    "email": "john@example.com",
    "name": "John Doe",
    "role": {
      "group": ["accountant", "moderator"]
    }  
  }
"""

assert {
  read[User](json) == User(
    email = "john@example.com",
    name = Some("Jonh Doe"),
    role = Role.Group(Role.Accountant, Role.Moderator)
  )
}    
```

### Case class default parameters

That if we add the new field to class and try to read JSON written to KV storage with an old version of the class? An exception will be thrown. To avoid this behavior add the new field with a default value.

```scala
@pushka case class User(
  email: String,
  name: Option[String],
  role: Role,
  photoUrl: String = "http://example.com/images/users/dafault.jpg"
)
```

### `@key` annotation

Pushka allows defining the key that a field is serialized with via a `@key` annotation.

```scala
@pushka
case class Log(@key("@ts") timestamp: String, message: String)
```

### `@forceObject` annotation

Case classes with one field write without object wrapper by default. To avoid this behavior use `@forceObject` annotation.

```scala
@pushka case class Id(value: String)
write(Id("9f3ce5")) // "9f3ce5"

@pushka @forceObject case class Id(value: String)
write(Id("9f3ce5")) // { "value": "9f3ce5" }
```

### `Map` writing

Obviously `Map[K, V]` should be written as `{}` and this is true when `K` is `String`, `Int`, `Double` or `UUID`. But several `K` types can't be written as JSON object key. Consider `case class Point(x: Int, y: Int)`. This type will be written to JSON object. In this case `Map[Point, T]` will be written as a sequence of tuples.

```scala
@pushka case class Point(x: Int, y: Int)

val m: Map[Point, String] = Map(
  Point(0,1) -> "John",
  Point(1,0) -> "Jane"
)

write(m)
```
```json
[
  [ { "x": 1, "y": 0 }, "John" ],
  [ { "x": 0, "y": 1 }, "Jane" ]
]
```

If you want to write such maps as `{}` you should prove that `K` type can be written as string.

```scala
@pushka case class Point(x: Int, y: Int)

object Point {
  implicit val pointOk = new puska.ObjectKey[Point] {
    def stringify(value: Point): String = s"${value.x}:${value.y}"
    def parse(s: String): Point = {
      val Array(x, y) = s.split(":")
      Point(x.toInt, y.toInt)
    }
  }
}

val m: Map[Point, String] = Map(
  Point(0,1) -> "John",
  Point(1,0) -> "Jane"
)

write(m)
```
```json
{
  "0:1": "John",
  "1:0": "Jane"
}
```

### Custom readers and writers

Sometimes we want to write objects in a special way.

```scala
import pushka.RW
import pushka.Ast

case class Name(first: String, last: String)

object Name {
  val Pattern = "(.*) (.*)".r
  implicit val rw = new pushka.RW[Name] {
    def write(value: Name): Ast = {
      Ast.Str(s"${value.first} ${value.last}")
    }
    def read(ast: Ast): Name = ast match {
      case Ast.Str(Pattern(first, last)) => Name(first, last)
      case _ => throw new Exception("It's wrong!")  
    }
  }
}

// ...

write(User("John", "Doe"))
```
```json
"John Doe"
```

# License

Code released under Apache 2.0 license. See [LICENSE](https://github.com/fomkin/pushka/blob/develop/LICENSE).
