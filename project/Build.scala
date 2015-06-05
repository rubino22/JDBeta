import sbt._
import Keys._

object JandomBuild extends Build {
  val pplJar = settingKey[Option[String]]("Location of the PPL library")
  val wrapperJar = settingKey[Option[String]]("Location of the Wrapper GMP library")
  val gitHeadCommitSHA = taskKey[String]("Current git commit SHA")
}

