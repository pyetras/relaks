name := "lang"

mainClass in (Compile, run) := Some("HelloTest")

libraryDependencies += "com.assembla.scala-incubator" %% "graph-core" % "1.9.1"

libraryDependencies += "com.googlecode.kiama" %% "kiama" % "2.0.0-SNAPSHOT"

libraryDependencies += "org.scalaz" %% "scalaz-core" % scalazVersion

libraryDependencies += "org.apache.drill" % "drill-common" % "1.1.0-SNAPSHOT"

dependencyOverrides ++= Set(
  "org.apache.calcite" % "calcite-core" % "1.2.0-incubating"
)

libraryDependencies += "org.apache.calcite" % "calcite-core" % "1.2.0-incubating"

libraryDependencies += "org.scalikejdbc" %% "scalikejdbc" % "2.2.6"

libraryDependencies += "com.bethecoder" % "ascii_table" % "1.0-SNAPSHOT"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.12-SNAPSHOT"

libraryDependencies += "org.scalanlp" %% "breeze-natives" % "0.12-SNAPSHOT"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"

resolvers += "Concurrent Maven Repo" at "http://conjars.org/repo"