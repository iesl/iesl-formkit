import sbt._
import sbt.Keys._
import sbt.Keys._


object IeslPluginLoader extends Build {
  
  lazy val root = Project(id = "plugins", base = file("."))
    .settings(resolvers += "IESL Public Releases" at "http://dev-iesl.cs.umass.edu/nexus/content/groups/public")
    .settings(resolvers += "IESL Public Snapshots" at "http://dev-iesl.cs.umass.edu/nexus/content/groups/public-snapshots")
    .settings(addSbtPlugin("edu.umass.cs.iesl" %% "iesl-sbt-base" % "latest.release"))
    .settings(addSbtPlugin("play" %% "sbt-plugin" % "2.1.2"))
    //.settings(addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.7.4"))
    //.settings(addSbtPlugin("com.mojolly.scalate" % "xsbt-scalate-generator" % "0.4.2"))
    //.settings(addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.4.0"))
    //.settings(addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.8.8"))
}
 
 
