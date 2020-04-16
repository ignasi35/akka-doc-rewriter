name := "doc-rewriter"

version := "1.0"

scalaVersion := "2.13.1"

lazy val akkaVersion = "2.6.4"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.lightbend.akka" %% "akka-stream-alpakka-file" % "2.0.0-RC2",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)
