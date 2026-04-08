name := "poker-scala"
version := "0.1.0-SNAPSHOT"
scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .enablePlugins(PlayScala)
  .settings(
    libraryDependencies ++= Seq(
      guice,
      "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.1" % Test,
      "org.scalatest"          %% "scalatest"           % "3.2.17" % Test
    ),
    // Fork a separate JVM for runMain so Play's sbt plugin doesn't interfere
    Compile / run / fork         := true,
    // Forward keyboard input from sbt to the forked process (needed for console game)
    Compile / run / connectInput := true
  )
