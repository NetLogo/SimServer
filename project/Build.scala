import
  sbt._,
    Keys._

import
  play.Project,
    Project._

object ApplicationBuild extends Build {

    val appName         = "SimServer"
    val appVersion      = "1.0-SNAPSHOT"

    resolvers += (
      "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
    )

    val appDependencies = Seq(
      anorm, jdbc,
      "org.scalaz" %% "scalaz-core" % "7.0.6",
      "mysql" % "mysql-connector-java" % "5.1.18",
      "org.bizzle.datastructure" % "DataStructure" % "8e9e5ce" from
        "https://ccl.northwestern.edu/devel/jason/DataStructure-8e9e5ce.jar"
    )

    val main = Project(appName, appVersion, appDependencies).settings(
      resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
      scalacOptions += "-language:_"
    )

}
