name := "poker-scala"
version := "0.1.0-SNAPSHOT"
scalaVersion := "2.13.12"

val pekkoVersion = "1.0.2"
val pekkoHttpVersion = "1.0.1"

libraryDependencies ++= Seq(
  "org.apache.pekko" %% "pekko-actor-typed" % pekkoVersion,
  "org.apache.pekko" %% "pekko-stream" % pekkoVersion,
  "org.apache.pekko" %% "pekko-http" % pekkoHttpVersion
)

lazy val root = (project in file("."))
  .enablePlugins(PlayScala)
  .settings(
    libraryDependencies ++= Seq(
      guice,
      "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.1" % Test,
      "org.scalatest"          %% "scalatest"           % "3.2.17" % Test
    ),

    Compile / mainClass := Some("network.MultiplayerApp"),
    Compile / run / mainClass := Some("network.MultiplayerApp"),
    // Fork a separate JVM for runMain so Play's sbt plugin doesn't interfere
    Compile / run / fork         := true,
    // Forward keyboard input from sbt to the forked process (needed for console game)
    Compile / run / connectInput := true
  )
