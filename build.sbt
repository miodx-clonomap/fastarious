Nice.scalaProject

name          := "fastarious"
organization  := "ohnosequences"
description   := "fastarious project"

bucketSuffix  := "era7.com"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.5" % Test

libraryDependencies ++= Seq(
  "ohnosequences" %% "cosas" % "0.8.0"
)
