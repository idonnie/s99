/** Project */
name := "s99"

version := "1.0"

organization := "org.specs2"

scalaVersion := "2.10.0-RC1"

/** Dependencies */
resolvers ++= Seq(
  "snapshots-repo" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
  "org.scala-tools.testing" % "test-interface" % "0.5",
  "org.specs2" %% "specs2-scalaz-core" % "6.0.1",
  "org.specs2" %% "specs2" % "1.13",
  "org.pegdown" % "pegdown" % "1.0.2"
)

scalacOptions ++= Seq("-deprecation", "-unchecked")

/** Console */
initialCommands in console := "import org.specs2._"

