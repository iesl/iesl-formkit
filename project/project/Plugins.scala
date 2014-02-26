import sbt._
import sbt.Keys._
import sbt.Keys._


object IeslPluginLoader extends Build {
  
  lazy val root = Project(id = "plugins", base = file("."))
    .settings(resolvers += "IESL Public Releases" at "http://dev-iesl.cs.umass.edu/nexus/content/groups/public")
    .settings(resolvers += "IESL Public Snapshots" at "http://dev-iesl.cs.umass.edu/nexus/content/groups/public-snapshots")
    .settings(addSbtPlugin("edu.umass.cs.iesl" %% "iesl-sbt-base" % "latest.release"))
    .settings(addSbtPlugin("com.typesafe.play" %% "sbt-plugin" % "2.2.1"))
    .settings(addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "0.6.3"))
}
 
 
