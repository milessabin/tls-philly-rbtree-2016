organization := "org.typelevel"

name := "tls-philly-rbtree"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "com.chuusai"    %% "shapeless"  % "2.3.0",
  "org.typelevel"  %% "cats"       % "0.4.1",
  "org.scalatest"  %% "scalatest"  % "3.0.0-M7" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
  "org.typelevel"  %% "discipline" % "0.4" % "test"
)

initialCommands in console :=
  """
    import shapeless._
  """

scalacOptions := Seq(
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked")
