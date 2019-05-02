name          := "fastarious"
organization  := "com.miodx.clonomap"
version       := "0.12.0"
description   := "FASTQ and FASTA APIs"
bucketSuffix  := "era7.com"

scalaVersion  := "2.12.8"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.4" % Test
)

// shows time for each test:
testOptions in Test += Tests.Argument("-oD")
// disables parallel exec
parallelExecution in Test := false
