import ReleaseTransformations._

lazy val Vers = new {
  val circe = "0.9.0"
  val scalatest = "3.0.5"
  val scalameta = "4.0.0"
}

lazy val commonSettings = Seq(
  name := "Argus",
  organization := "com.github.aishfenton",
  scalaVersion := "2.12.6",
  crossScalaVersions := Seq("2.11.12", "2.12.2"),
  scalacOptions ++= Seq("-target:jvm-1.8", "-Ypartial-unification"),
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
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
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
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    releaseStepCommand("sonatypeReleaseAll"),
    //ReleaseStep(action = Command.process("sonatypeReleaseAll", _), enableCrossBuild = true),
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
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided,

      "io.circe" %% "circe-core" % Vers.circe,
      "io.circe" %% "circe-generic" % Vers.circe,
      "io.circe" %% "circe-parser" % Vers.circe,

      "org.scalameta" %% "scalameta" % Vers.scalameta,
      "org.scalameta" %% "contrib" % Vers.scalameta,

      "org.scalactic" %% "scalactic" % Vers.scalatest % Test,
      "org.scalatest" %% "scalatest" % Vers.scalatest % Test
    )
  )

lazy val root = (project in file(".")).
  aggregate(argus).
  settings(commonSettings: _*).
  settings(noPublishSettings: _*)


// Clears screen between refreshes in continuous mode
maxErrors := 5
triggeredMessage := Watched.clearWhenTriggered

