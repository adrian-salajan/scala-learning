name := "Exercises"

version := "0.1"

scalaVersion := "2.13.3"
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)



val catsVersion = "2.2.0"

libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion
libraryDependencies += "org.typelevel" %% "cats-kernel" % catsVersion
libraryDependencies += "org.typelevel" %% "alleycats-core" % catsVersion
libraryDependencies += "org.typelevel" %% "cats-effect" % catsVersion


libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"


val AkkaVersion = "2.6.10"
libraryDependencies += "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion
libraryDependencies += "com.typesafe.akka" %% "akka-stream-typed" % AkkaVersion

libraryDependencies += "com.typesafe.akka" %% "akka-remote" % AkkaVersion
libraryDependencies += "com.typesafe.akka" %% "akka-cluster-typed" % AkkaVersion
libraryDependencies += "com.typesafe.akka" %% "akka-cluster-sharding-typed" % AkkaVersion
//libraryDependencies += "com.typesafe.akka" %% "akka-cluster-singleton" % AkkaVersion
libraryDependencies += "com.typesafe.akka" %% "akka-persistence-typed" % AkkaVersion

val AkkaHttpVersion = "10.2.1"
libraryDependencies +=  "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion
//libraryDependencies +=  "com.typesafe.akka" %% "akka-stream" % AkkaVersion

libraryDependencies += "co.fs2" %% "fs2-core" % "2.4.4" // For cats 2 and cats-effect 2


