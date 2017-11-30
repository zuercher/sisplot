name := "sisplot"

version := "1.0"

scalaVersion := "2.12.3"

libraryDependencies ++= Seq(
  "com.twitter" % "util-app_2.12"  % "7.0.0",
  "com.twitter" % "util-core_2.12"  % "7.0.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "org.scalatest" % "scalatest_2.12" % "3.0.4" % "test"
)
