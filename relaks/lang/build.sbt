name := "lang"

mainClass in (Compile, run) := Some("HelloTest")

libraryDependencies += "com.assembla.scala-incubator" %% "graph-core" % "1.9.1"

libraryDependencies += "com.googlecode.kiama" %% "kiama" % "1.8.0"

libraryDependencies += "org.scalaz" %% "scalaz-core" % scalazVersion
