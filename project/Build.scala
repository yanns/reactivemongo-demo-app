import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "mongo-app"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      "org.reactivemongo" %% "play2-reactivemongo" % "0.9",
      "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
    )

    val main = play.Project(appName, appVersion, appDependencies).settings(
    )

}
