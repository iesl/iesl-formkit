import sbt._
import Keys._
//import edu.umass.cs.iesl.sbtbase.Dependencies
//import edu.umass.cs.iesl.sbtbase.IeslProject._
import edu.umass.cs.iesl.sbtbase.{IeslProject => Iesl, Config=>IeslConfig}

import play.Play.autoImport._
import play.Play._
import PlayKeys._

object IeslFormKitBuild extends Build {

  val scalaV = "2.11.2"
  val vers = "0.1-SNAPSHOT"
  val org = "edu.umass.cs.iesl"

  // implicit val allDeps: Dependencies = new Dependencies()

  // import allDeps._
  
  val deps = Seq(
    // todo : figure out which of these are not actually used locally
    // (I had to include them explicitly due to mysterious issues with transitive resolution)

    //"net.openreview" %% "scalate-core" % "latest.integration",
    //"net.openreview" %% "scalate-core-plus" % "latest.integration",
    //"net.openreview" %% "scalate-util" % "latest.integration",
    // 
    "edu.umass.cs.iesl" %% "scalacommons" % "latest.integration"
  )

  lazy val root = ((project in file(".")).enablePlugins(play.PlayScala)
    .settings(Iesl.scalaSettings(Iesl.DebugVars):_*)
    // .ieslSetup(vers, deps, Public, WithSnapshotDependencies, org = organization, conflict = ConflictStrict)
    //.settings(releaseSettings:_*)
    .settings(
      name := "iesl-formkit",
      organization := org,
      Iesl.setConflictStrategy(Iesl.ConflictStrict),
      scalaVersion := scalaV,
      libraryDependencies ++= deps
    )//.cleanLogging.standardLogging)
  )
}
