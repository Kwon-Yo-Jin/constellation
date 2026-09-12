name := "constellation"
version := "0.1"
scalaVersion := "2.13.12"

scalacOptions ++= Seq(
  "-language:reflectiveCalls",
  "-deprecation",
  "-feature"
)

libraryDependencies ++= Seq(
  "org.chipsalliance" %% "chisel" % "6.7.0",
  "edu.berkeley.cs" %% "chiseltest" % "6.0.0" % "test",
  "com.lihaoyi" %% "sourcecode" % "0.3.1",
  "com.lihaoyi" %% "mainargs" % "0.5.0",
  "org.json4s" %% "json4s-jackson" % "4.0.5"
)

addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % "6.7.0" cross CrossVersion.full)

Compile / unmanagedSourceDirectories ++= Seq(
  baseDirectory.value / ".." / "rocket-chip" / "dependencies" / "cde" / "cde" / "src",
  baseDirectory.value / ".." / "rocket-chip" / "dependencies" / "diplomacy" / "diplomacy" / "src",
  baseDirectory.value / ".." / "rocket-chip" / "dependencies" / "hardfloat" / "hardfloat" / "src" / "main" / "scala",
  baseDirectory.value / ".." / "rocket-chip" / "src" / "main" / "scala"
)

Compile / unmanagedResourceDirectories +=
  baseDirectory.value / ".." / "rocket-chip" / "src" / "main" / "resources"

import Tests._

Test / fork := true
Test / testGrouping := (Test / testGrouping).value.flatMap { group =>
   group.tests.map { test =>
      Group(test.name, Seq(test), SubProcess(ForkOptions()))
   }
}
concurrentRestrictions := Seq(Tags.limit(Tags.ForkedTestGroup, 4))

