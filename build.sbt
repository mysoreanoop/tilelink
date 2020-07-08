// See LICENSE.Berkeley for license details.

import sbt.complete.DefaultParsers._
import scala.sys.process._

lazy val commonSettings = Seq(
  organization := "Morphing Machines Pvt. Ltd.",
  version      := "0.0",
  scalaVersion := "2.12.10",
  parallelExecution in Global := false,
  traceLevel   := 15,
  scalacOptions ++= Seq("-deprecation","-unchecked","-Xsource:2.11"),
  libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value),
  libraryDependencies ++= Seq("org.json4s" %% "json4s-jackson" % "3.6.1"),
  libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.0.8" % "test"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")
  )
)

//lazy val chisel = (project in file("rocket-chip/chisel3")).settings(commonSettings)

def dependOnChisel(prj: Project) = {
  //if (sys.props.contains("ROCKET_USE_MAVEN")) {
    prj.settings(
      libraryDependencies ++= Seq("edu.berkeley.cs" %% "chisel3" % "3.3-SNAPSHOT")
    )
  //} else {
  //  prj.dependsOn(chisel)
  //}
}

lazy val `api-config-chipsalliance` = (project in file("rocket-chip/api-config-chipsalliance/build-rules/sbt"))
  .settings(commonSettings)
lazy val hardfloat  = dependOnChisel(project in file("rocket-chip/hardfloat")).settings(commonSettings)
lazy val `rocket-macros` = (project in file("rocket-chip/macros")).settings(commonSettings)
lazy val rocketchip = dependOnChisel(project in file("rocket-chip"))
  .settings(commonSettings)
  .dependsOn(`api-config-chipsalliance` % "compile-internal;test-internal")
  .dependsOn(hardfloat % "compile-internal;test-internal")
  .dependsOn(`rocket-macros` % "compile-internal;test-internal")
  
lazy val sifiveBlocks = dependOnChisel(project in file("sifive-blocks"))
  .dependsOn(rocketchip)
  .settings(commonSettings) 		

lazy val fpgaShells = dependOnChisel(project in file("fpga-shells"))
  .dependsOn(rocketchip, sifiveBlocks)
  .settings(commonSettings) 		

lazy val testchipip = dependOnChisel(project in file("testchipip"))
  .settings(commonSettings)
  .dependsOn(rocketchip, sifiveBlocks)

val defaultVersions = Map(
  "chisel-iotesters" -> "1.4-SNAPSHOT",
  "chiseltest" -> "0.2-SNAPSHOT",
  "firrtl-diagrammer" -> "1.2-SNAPSHOT"
  )

lazy val top = dependOnChisel(project in file("."))
  .settings(commonSettings,
  libraryDependencies ++= Seq("chisel-iotesters","chiseltest","firrtl-diagrammer").map{
    dep: String => "edu.berkeley.cs" %% dep % defaultVersions(dep)})
  .dependsOn(rocketchip, testchipip, sifiveBlocks, fpgaShells)

