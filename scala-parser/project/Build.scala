/**
 * Created by Pietras on 23/04/15.
 */
import sbt._
import Keys._

object BuildProject extends Build {
  lazy val fwb = Project(id = "fwb", base = file(".")).settings(basicSettings).aggregate(parser)

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
      "org.scalaz" %% "scalaz-core" % scalazVersion,
      "org.scalaz" %% "scalaz-effect" % scalazVersion,
      "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
      "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test",
      "org.scala-lang" % "scala-reflect" % _scalaVersion,
      "com.chuusai" %% "shapeless" % "2.2.0-RC4",
      "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
    )
  )

  lazy val parser = project.in(file("parser")).settings(commonSettings:_*)
}