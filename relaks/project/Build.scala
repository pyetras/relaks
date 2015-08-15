/**
 * Created by Pietras on 23/04/15.
 */
import sbt._
import Keys._

object BuildRelaks extends Build {
  lazy val relaks = Project(id = "relaks", base = file(".")).settings(basicSettings ++ Seq(name := "relaks")).aggregate(lang, optimizer, data)

  val scalazVersion = "7.1.3"
  val _scalaVersion = "2.11.7"

  val mavenLocal = "Local Maven Repository" at Path.userHome.asFile.toURI.toURL + "/.m2/repository"

  val basicSettings = Project.defaultSettings ++ Seq(
    version := "1.0",
    scalaVersion := _scalaVersion,
    scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked")
  )

  val commonSettings = basicSettings ++ Seq(
    resolvers ++= Seq(
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots"),
      mavenLocal
    ),
    libraryDependencies ++= Seq(
//      "org.scalaz" %% "scalaz-core" % scalazVersion,
//      "org.scalaz" %% "scalaz-effect" % scalazVersion,
//      "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
//      "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
      "ch.qos.logback" % "logback-classic" % "1.1.3",

      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
      "org.scala-lang.modules" %% "scala-xml" % "1.0.3",
      "jline" % "jline" % "2.12.1",

      "org.scala-lang" % "scala-reflect" % _scalaVersion,
      "com.chuusai" %% "shapeless" % "2.2.3",
      "org.scalatest" %% "scalatest" % "2.2.4" % "test"
    ),
    dependencyOverrides ++= Set(
      "org.scala-lang.modules" %% "scala-xml" % "1.0.4"
    )

  )

  lazy val lang = project.in(file("lang")).settings(commonSettings:_*).dependsOn(optimizer, data)
  lazy val optimizer = project.in(file("optimizer")).settings(commonSettings:_*).dependsOn(data)
    .configs(FastTest)
    .settings(inConfig(FastTest)(Defaults.testTasks): _*)
    .settings(testOptions in FastTest := Seq(Tests.Argument("-l", "Slowpoke")))
  lazy val data = project.in(file("data")).settings(commonSettings:_*)

  lazy val FastTest = config("fast") extend Test
}