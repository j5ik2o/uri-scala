import Dependencies._
import Dependencies.Versions._

ThisBuild / scalafixScalaBinaryVersion := CrossVersion.binaryScalaVersion(scalaVersion.value)

def crossScalacOptions(scalaVersion: String): Seq[String] =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((3L, _)) =>
      Seq(
        "-source:3.0-migration",
        "-Xignore-scala2-macros"
      )
    case Some((2L, scalaMajor)) if scalaMajor >= 12 =>
      Seq(
        "-Ydelambdafy:method",
        "-target:jvm-1.8",
        "-Yrangepos",
        "-Ywarn-unused"
      )
  }

lazy val baseSettings = Seq(
  organization := "com.github.j5ik2o",
  homepage := Some(url("https://github.com/j5ik2o/uri-scala")),
  licenses := List("The MIT License" -> url("http://opensource.org/licenses/MIT")),
  developers := List(
    Developer(
      id = "j5ik2o",
      name = "Junichi Kato",
      email = "j5ik2o@gmail.com",
      url = url("https://blog.j5ik2o.me")
    )
  ),
  scalaVersion := Versions.scala213Version,
  // crossScalaVersions ++= Seq(Versions.scala213Version, Versions.scala3Version),
  scalacOptions ++= (
    Seq(
      "-unchecked",
      "-feature",
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-language:_"
    ) ++ crossScalacOptions(scalaVersion.value)
  ),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots")
  ),
  semanticdbEnabled := true,
  semanticdbVersion := scalafixSemanticdb.revision,
  Test / publishArtifact := false,
  Test / fork := true,
  Test / parallelExecution := false,
  Compile / doc / sources := {
    val old = (Compile / doc / sources).value
    if (scalaVersion.value == scala3Version) {
      Nil
    } else {
      old
    }
  }
)

lazy val library = (project in file("library"))
  .settings(baseSettings)
  .settings(
    name := "uri-scala",
    libraryDependencies ++= Seq(
      ("com.lihaoyi"      %% "fastparse"       % "3.0.1").cross(CrossVersion.for3Use2_13),
      "org.scalatest"     %% "scalatest"       % "3.2.15"   % Test,
      "org.scalatestplus" %% "scalacheck-1-15" % "3.2.11.0" % Test
    )
  )

lazy val example = (project in file("example"))
  .settings(baseSettings)
  .settings(
    name := "uri-example"
  ).dependsOn(library)

lazy val root = (project in file("."))
  .settings(baseSettings)
  .settings(
    name := "uri-scala-root",
    publish / skip := true
  )
  .aggregate(library)

// --- Custom commands
addCommandAlias("lint", ";scalafmtCheck;test:scalafmtCheck;scalafmtSbtCheck;scalafixAll --check")
addCommandAlias("fmt", ";scalafmtAll;scalafmtSbt")
