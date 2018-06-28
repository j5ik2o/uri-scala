name := "uris"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "com.lihaoyi"    %% "fastparse"  % "1.0.0",
  "org.parboiled"  %% "parboiled"  % "2.1.4",
  "org.typelevel"  %% "cats-core"  % "1.1.0",
  "org.scalatest"  %% "scalatest"  % "3.0.5" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)
