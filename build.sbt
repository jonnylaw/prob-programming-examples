name := "probabilistic-programming"

organization := "com.github.jonnylaw"

version := "0.1"

scalaVersion := "2.12.7"

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  Resolver.sonatypeRepo("public")
)

// this is for simulacrum typeclass support
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze" % "0.13.2",
  "org.scalanlp" %% "breeze-viz" % "0.13.2",
  "com.github.fommil.netlib" % "all" % "1.1.2",
  "org.typelevel" %% "spire" % "0.14.1",
  "com.nrinaudo" %% "kantan.csv-cats" % "0.4.0",
  "com.stripe" %% "rainier-cats" % "0.2.2",
  "org.typelevel" %% "cats-core" % "1.4.0",
  "org.typelevel" %% "cats-effect" % "1.3.1",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)
