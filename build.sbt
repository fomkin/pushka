resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

val commonSettings = Seq(
  scalaVersion := "2.11.7",
  organization := "com.github.fomkin",
  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.0-M7" % "test"
)

lazy val core = crossProject.crossType(CrossType.Pure).
  settings(commonSettings:_*).
  settings(
    normalizedName := "pushka-core",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
  )

lazy val coreJS = core.js
lazy val coreJVM = core.jvm

lazy val json = crossProject.crossType(CrossType.Full).
  settings(commonSettings:_*).
  settings(
    normalizedName := "pushka-json",
    unmanagedSourceDirectories in Test += baseDirectory.value / ".." / "test-src",
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
  ).
  jvmSettings(libraryDependencies += "org.spire-math" %% "jawn-parser" % "0.8.3").
  dependsOn(core)

lazy val jsonJS = json.js
lazy val jsonJVM = json.jvm
