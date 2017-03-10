import ReleaseTransformations._

lazy val Vers = new {
  val circe = "0.7.0"
  val scalatest = "2.2.6"
}

lazy val commonSettings = Seq(
  name := "Argus",
  organization := "com.github.aishfenton",
  scalaVersion := "2.11.8",
  scalacOptions += "-target:jvm-1.7",
  homepage := Some(url("https://github.com/aishfenton/Argus")),
  licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/MIT")),

  addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full),

  // NB: We put example schemas in main package since otherwise the macros can't run for test (since they
  // excute before test-classes is populated). But then we need to exclude them from packing.
  mappings in (Compile, packageBin) ~= { _.filter(!_._1.getName.endsWith(".json")) },

  publishMavenStyle := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  sonatypeProfileName := "com.github.aishfenton",
  pomExtra := (
    <scm>
      <url>git@github.com:aishfenton/Argus.git</url>
      <connection>scm:git:git@github.com:aishfenton/Argus.git</connection>
    </scm>
      <developers>
        <developer>
          <id>aishfenton</id>
          <name>Aish Fenton</name>
        </developer>
        <developer>
          <id>datamusing</id>
          <name>Sudeep Das</name>
        </developer>
        <developer>
          <id>dbtsai</id>
          <name>DB Tsai</name>
        </developer>
        <developer>
          <id>rogermenezes</id>
          <name>Roger Menezes</name>
        </developer>
      </developers>
  ),
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    ReleaseStep(action = Command.process("publishSigned", _)),
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges
  )

)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val argus = project.
  settings(moduleName := "argus").
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",

      "io.circe" %% "circe-core" % Vers.circe,
      "io.circe" %% "circe-generic" % Vers.circe,
      "io.circe" %% "circe-parser" % Vers.circe,

      "org.scalactic" %% "scalactic" % Vers.scalatest % "test",
      "org.scalatest" %% "scalatest" % Vers.scalatest % "test"
    )
  )

lazy val root = (project in file(".")).
  aggregate(argus).
  settings(commonSettings: _*).
  settings(noPublishSettings: _*)


// Clears screen between refreshes in continuous mode
maxErrors := 5
triggeredMessage := Watched.clearWhenTriggered

