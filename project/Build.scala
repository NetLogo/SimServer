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
      "org.scalaz" %% "scalaz-core" % "7.0-SNAPSHOT",
      "mysql" % "mysql-connector-java" % "5.1.18"
    )

    val main = Project(appName, appVersion, appDependencies).settings(
      resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
      scalacOptions += "-language:_"
    )

}
