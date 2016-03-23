name := "WeKanban"

version := "1.0"

scalaVersion := "2.10.0"

scalacOptions ++= Seq("-unchecked", "-deprecation")

scalaSource in Compile := baseDirectory.value / "src"/ "main" / "scala"

scalaSource in Test := baseDirectory.value / "src"/ "test" / "scala"

mainClass := Some("scala.db.KanbanSchema")

//libraryDependencies += "javax.servlet" % "javax.servlet-api" % "3.0.1" % "provided"
libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-http" % scalazVersion,
  "org.eclipse.jetty" % "jetty-servlet" % jettyVersion % "container",
  "org.eclipse.jetty" % "jetty-webapp" % jettyVersion % "test,container",
  "org.eclipse.jetty" % "jetty-server" % jettyVersion % "container",
   "com.h2database" % "h2" % "1.2.137",
   "org.squeryl" % "squeryl_2.10" % "0.9.5-6"
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
