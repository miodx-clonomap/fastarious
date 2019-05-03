name          := "fastarious"
organization  := "com.miodx.common"
version       := "0.11.0"
description   := "FASTQ and FASTA APIs"
scalaVersion  := "2.11.11"

bucketSuffix  := "era7.com"

libraryDependencies ++= Seq(
  "com.miodx.common" %% "cosas" % "0.10.1",
  "org.spire-math"   %% "spire" % "0.13.0"
)

// NOTE should be reestablished
wartremoverErrors in (Test,    compile) := Seq()
wartremoverErrors in (Compile, compile) := Seq()

// shows time for each test:
testOptions in Test += Tests.Argument("-oD")
// disables parallel exec
parallelExecution in Test := false
