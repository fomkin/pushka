resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

val pushkaVersion = "0.1.0-SNAPSHOT"

val publishSettings = if (pushkaVersion.endsWith("SNAPSHOT")) {
  Seq(
    publishTo := Some("Flexis Thirdparty Snapshots" at "https://nexus.flexis.ru/content/repositories/thirdparty-snapshots"),
    credentials += {
      val ivyHome = sys.props.get("sbt.ivy.home") match {
        case Some(path) ⇒ file(path)
        case None ⇒ Path.userHome / ".ivy2"
      }
      Credentials(ivyHome / ".credentials")
    }
  )
}
else {
  Seq()
}

val commonSettings = publishSettings ++ Seq(
  scalaVersion := "2.11.7",
  organization := "com.github.fomkin",
  version := pushkaVersion,
  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.0-M7" % "test",
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-Xfatal-warnings",
    "-language:postfixOps",
    "-language:implicitConversions"
  )
)

lazy val core = crossProject.crossType(CrossType.Pure).
  settings(commonSettings: _*).
  settings(
    normalizedName := "pushka-core",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full),
    sourceGenerators in Compile <+= sourceManaged in Compile map GenTuples
  )


lazy val coreJS = core.js
lazy val coreJVM = core.jvm

lazy val json = crossProject.crossType(CrossType.Full).
  settings(commonSettings: _*).
  settings(
    normalizedName := "pushka-json",
    unmanagedSourceDirectories in Test += baseDirectory.value / ".." / "test-src",
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
  ).
  jvmSettings(libraryDependencies += "org.spire-math" %% "jawn-parser" % "0.8.3").
  dependsOn(core)

lazy val jsonJS = json.js
lazy val jsonJVM = json.jvm

// Do not publish root project
publish := {}
