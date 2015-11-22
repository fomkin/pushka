# Pushka

[![Build Status](https://travis-ci.org/fomkin/pushka.svg?branch=develop)](https://travis-ci.org/fomkin/pushka)

Pushka is a pickler implemented without any runtime reflection. It created to reach well human readability of output JSON and good performance. Pushka works well both on Scala and Scala.js.

# Motivation

1. Most of picklers are writes case classes "as is". For example if we have `Option` value it will be writeen with some kind of wrapper. In the case of sealed traits, most picklers writes metadata: trait name and case class name. This make JSON unreadable by human and make it usless for creating public API. We want to achive high human readablility of output JSON: no wrappers if it possible, no metadata ever.

2. Codebase simplicity. In our work we encountered that some picklers based on implicit macros (including shapeless base picklers) are fails on our data. In this project we want make code as simple as possible to find bugs faster.

3. High performance. Minimum runtime overhead. See [Boopickle benchmarks](http://ochrons.github.io/boopickle-perftest/).


# Usage

Add Pushka dependency to your project.

```scala
// For Scala.js
libraryDependencies += "com.github.fomkin" %%% "pushka-json" % "0.3.2"

// For Scala.jvm
libraryDependencies += "com.github.fomkin" %% "pushka-json" % "0.3.2"
```
Pushka uses marco annotations which implemented in macro paradise plugin. Unfortunately it can't be added transitively by Pushka dependency, so you need to plug it manually.

```scala
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
```
Let define types we want to write to JSON.

```scala
import pushka.annotation._

@pushka 
case class User(email: String, name: Option[String], role: Role)

@puska
sealed trait Role

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
Now, in the opposite direction. Lets read JSON.
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

That if we add new field to class and try to read JSON written to KV storage with an old version of the class? An exception will be thrown. To avoid this behavior add new filed with default value.

```scala
@pushka 
case class User(
  email: String, 
  name: Option[String], 
  role: Role, 
  photoUrl: String = "http://example.com/images/users/dafault.jpg"
)
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
### Write `None` as `null`

You can configure Pushka to write `None` explictly.

```scala
@pushka 
case class User(email: String, name: Option[String])

implicit val config = pushka.Config(leanOptions = false)
write(User("john@example.com", None))
```
```json
{ "email": "john@example.com", "name": null }
```

# License

Code released under Apache 2.0 license. See [LICENSE](https://github.com/fomkin/pushka/blob/develop/LICENSE). 
