import sbtcrossproject.{crossProject, CrossType}

val publishSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value) Some("snapshots" at s"${nexus}content/repositories/snapshots")
    else Some("releases" at s"${nexus}service/local/staging/deploy/maven2")
  },
  pomExtra := {
    <url>https://github.com/fomkin/pushka</url>
    <licenses>
      <license>
        <name>Apache License, Version 2.0</name>
        <url>http://apache.org/licenses/LICENSE-2.0</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:fomkin/pushka.git</url>
      <connection>scm:git:git@github.com:fomkin/pushka.git</connection>
    </scm>
    <developers>
      <developer>
        <id>fomkin</id>
        <name>Aleksey Fomkin</name>
        <email>aleksey.fomkin@gmail.com</email>
      </developer>
    </developers>
  }
)

val commonSettings = publishSettings ++ Seq(
  organization := "com.github.fomkin",
  version := "0.9.0-SNAPSHOT",
  scalaVersion := "2.11.8",
  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.0" % "test",
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-Xfatal-warnings",
    "-language:postfixOps",
    "-language:implicitConversions"
  )
)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform).
  crossType(CrossType.Pure).
  settings(commonSettings: _*).
  settings(
    normalizedName := "pushka-core",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "macro-compat" % "1.1.1",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    sourceGenerators in Compile <+= sourceManaged in Compile map GenTuples
  ).  
  nativeSettings(libraryDependencies -= "org.scalatest" %%% "scalatest" % "3.0.0" % "test")

lazy val coreJS = core.js
lazy val coreJVM = core.jvm
lazy val coreNative = core.native

lazy val json = crossProject(JSPlatform, JVMPlatform, NativePlatform).
  crossType(CrossType.Full).
  settings(commonSettings: _*).
  settings(
    normalizedName := "pushka-json",
    unmanagedSourceDirectories in Test += baseDirectory.value / ".." / "test-src"
  ).
  jvmSettings(libraryDependencies += "org.spire-math" %% "jawn-parser" % "0.10.4").
  nativeSettings(
    libraryDependencies += "org.json4s" %% "json4s-native" % "3.5.1",
    libraryDependencies -= "org.scalatest" %%% "scalatest" % "3.0.0" % "test"
  ).
  dependsOn(core).
  nativeSettings()

lazy val jsonJS = json.js
lazy val jsonJVM = json.jvm
lazy val jsonNative = json.native

publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))

crossScalaVersions := Seq("2.11.8", "2.12.1")

publishArtifact := false
