name := "optimizer"

resolvers += "Scalaz Bintray Repo" at "https://dl.bintray.com/scalaz/releases"

libraryDependencies += "org.scalaz.stream" %% "scalaz-stream" % "0.7a"

libraryDependencies += "org.scalaz" %% "scalaz-core" % scalazVersion

libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % scalazVersion

libraryDependencies += "log4j" % "log4j" % "1.2.17"
