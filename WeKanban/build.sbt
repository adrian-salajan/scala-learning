name := "WeKanban"

version := "1.0"

scalaVersion := "2.10.0"

scalacOptions ++= Seq("-unchecked", "-deprecation")



//libraryDependencies += "javax.servlet" % "javax.servlet-api" % "3.0.1" % "provided"
libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "6.0.3",
  "org.scalaz" %% "scalaz-http" % "6.0.3",
  "org.eclipse.jetty" % "jetty-servlet" % "7.3.0.v20110203" % "container",
  "org.eclipse.jetty" % "jetty-webapp" % "7.3.0.v20110203" % "test,container",
  "org.eclipse.jetty" % "jetty-server" % "7.3.0.v20110203" % "container"
  )



//val consumeIt = enablePlugins(JettyPlugin)
//http://stackoverflow.com/questions/34404558/intellij-idea-and-sbt-syntax-error
lazy val root = (project in file(".")).
  enablePlugins(JettyPlugin).
  settings(
    name := "test",
    scalaVersion := "2.10.0",
    version := "1.0"
  )
