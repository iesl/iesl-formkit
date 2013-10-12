import _root_.sbtassembly.Plugin.AssemblyKeys
import edu.umass.cs.iesl.sbtbase.IeslProject.ConflictStrict
import edu.umass.cs.iesl.sbtbase.IeslProject.Public
import edu.umass.cs.iesl.sbtbase.IeslProject.WithSnapshotDependencies
import sbt._
import Keys._
import edu.umass.cs.iesl.sbtbase.Dependencies
import edu.umass.cs.iesl.sbtbase.IeslProject._
import sbt.Keys._
import Keys._
import edu.umass.cs.iesl.sbtbase.Dependencies
import edu.umass.cs.iesl.sbtbase.IeslProject._

import sbtassembly.Plugin._
import AssemblyKeys._

import play.Project._

object IeslFormKitBuild extends Build {

  val vers = "0.1-SNAPSHOT"
  val organization = "edu.umass.cs.iesl"

  implicit val allDeps: Dependencies = new Dependencies()

  import allDeps._
  
  val deps = Seq(
    // todo : figure out which of these are not actually used locally
    // (I had to include them explicitly due to mysterious issues with transitive resolution)

    "net.openreview" %% "scalate-core" % "latest.integration",
    "net.openreview" %% "scalate-core-plus" % "latest.integration",
    "net.openreview" %% "scalate-util" % "latest.integration",
   
    ieslScalaCommons("latest.integration")
  )

  lazy val ieslFormKit = {
    (play.Project("iesl-formkit", vers, path = file("."))
      .ieslSetup(vers, deps, Public, WithSnapshotDependencies, org = organization, conflict = ConflictStrict)
      .cleanLogging.standardLogging
      )
  }
}
