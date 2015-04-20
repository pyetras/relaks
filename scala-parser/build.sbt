name := "scala-parser"

version := "1.0"

scalaVersion := "2.11.6"

val scalazVersion = "7.1.1"

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
  "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test"
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.1.0"
)

libraryDependencies += "com.assembla.scala-incubator" %% "graph-core" % "1.9.1"

libraryDependencies += "com.googlecode.kiama" % "kiama_2.11" % "1.8.0"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked")
