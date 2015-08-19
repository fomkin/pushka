resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

lazy val core = (crossProject.crossType(CrossType.Full) in file(".")).
  settings(
    scalaVersion := "2.11.7",
    organization := "com.github.fomkin",
    normalizedName := "pushka",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
      "org.scalatest" % "scalatest_2.11" % "3.0.0-M7" % "test"
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
  )

lazy val coreJS = core.js
lazy val coreJVM = core.jvm
