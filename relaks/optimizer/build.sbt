name := "optimizer"

resolvers += "Scalaz Bintray Repo" at "https://dl.bintray.com/scalaz/releases"

libraryDependencies += "org.scalaz.stream" %% "scalaz-stream" % "0.7a"

libraryDependencies += "org.scalaz" %% "scalaz-core" % scalazVersion

libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % scalazVersion

libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.3.0.RC2"
