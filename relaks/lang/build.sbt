name := "lang"

mainClass in (Compile, run) := Some("HelloTest")

libraryDependencies += "com.assembla.scala-incubator" %% "graph-core" % "1.9.1"

libraryDependencies += "com.googlecode.kiama" %% "kiama" % "1.8.0"

libraryDependencies += "org.scalaz" %% "scalaz-core" % scalazVersion

libraryDependencies += "org.apache.drill" % "drill-common" % "1.1.0-SNAPSHOT"

dependencyOverrides ++= Set(
  "org.apache.calcite" % "calcite-core" % "1.2.0-incubating"
)

libraryDependencies += "org.apache.calcite" % "calcite-core" % "1.2.0-incubating"

libraryDependencies += "org.scalikejdbc" %% "scalikejdbc" % "2.2.6"