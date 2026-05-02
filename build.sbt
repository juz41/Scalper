ThisBuild / version := "0.2.1-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

scalaJSUseMainModuleInitializer := true
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.4.0"
Compile / unmanagedSourceDirectories += baseDirectory.value / "src" / "Game" / "scala"

enablePlugins(ScalaJSPlugin)

lazy val root = (project in file("."))
  .settings(
    name := "Scalper"
  )
