/**
 * Created by Pietras on 23/04/15.
 */
import sbt._
import Keys._

object BuildRelaks extends Build {
  lazy val relaks = Project(id = "relaks", base = file(".")).settings(basicSettings ++ Seq(name := "relaks")).aggregate(lang, optimizer)

  val scalazVersion = "7.1.1"
  val _scalaVersion = "2.11.6"

  val basicSettings = Project.defaultSettings ++ Seq(
    version := "1.0",
    scalaVersion := _scalaVersion,
    scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked")
  )

  val commonSettings = basicSettings ++ Seq(
    resolvers ++= Seq(
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),
    libraryDependencies ++= Seq(
//      "org.scalaz" %% "scalaz-core" % scalazVersion,
//      "org.scalaz" %% "scalaz-effect" % scalazVersion,
//      "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
//      "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test",
      "org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.3",
      "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.3",
      "jline" % "jline" % "2.12.1",

      "org.scala-lang" % "scala-reflect" % _scalaVersion,
      "com.chuusai" %% "shapeless" % "2.2.0-RC4",
      "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
    )
  )

  lazy val lang = project.in(file("lang")).settings(commonSettings:_*).dependsOn(optimizer)
  lazy val optimizer = project.in(file("optimizer")).settings(commonSettings:_*)
}