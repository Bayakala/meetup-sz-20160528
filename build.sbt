name := "meetup-sz"

version := "1.0"

scalaVersion := "2.11.7"

val scalazVersion = "7.2.2"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
  "org.scalaz" %% "scalaz-typelevel" % "7.1.0",
  "org.scalaz.stream" %% "scalaz-stream" % "0.8a",
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test",
  "com.chuusai" %% "shapeless" % "2.2.5",
  "org.typelevel" %% "shapeless-scalacheck" % "0.4",
  "org.typelevel" %% "shapeless-spire" % "0.4",
  "org.typelevel" %% "shapeless-scalaz" % "0.4",
  "ch.qos.logback" % "logback-classic" % "1.0.13",
  "postgresql" % "postgresql" % "9.1-901.jdbc4",
  "com.jolbox" % "bonecp" % "0.8.0.RELEASE"
)

scalacOptions += "-feature"

initialCommands in console := "import scalaz._, Scalaz._, shapeless._"

initialCommands in console in Test := "import scalaz._, Scalaz._, scalacheck.ScalazProperties._, scalacheck.ScalazArbitrary._,scalacheck.ScalaCheckBinding._"

