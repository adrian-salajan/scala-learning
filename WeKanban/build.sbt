name := "WeKanban"

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies += "javax.servlet" % "javax.servlet-api" % "3.0.1" % "provided"

//val consumeIt = enablePlugins(JettyPlugin)
//http://stackoverflow.com/questions/34404558/intellij-idea-and-sbt-syntax-error
lazy val root = (project in file(".")).
  enablePlugins(JettyPlugin).
  settings(
    name := "test",
    scalaVersion := "2.11.7",
    version := "1.0"
  )
