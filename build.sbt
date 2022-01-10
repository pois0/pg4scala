name := "pg4scala"
ThisBuild / organization := "jp.pois"
version := sys.env.getOrElse("PG4SCALA_VERSION", "1.0.0-SNAPSHOT")

scmInfo := Some(
  ScmInfo(
    url("https://github.com/pois0/pg4scala"),
    "scm:git@github.com:pois0/pg4scala.git"))
developers := List(
  Developer(id="pois0", name="Shusuke Takahashi", email="dev@pois.jp", url=url("https://github.com/pois0"))
)

scalaVersion := "2.12.13"

idePackagePrefix := Some("jp.pois.pg4scala")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4"

githubOwner := "pois0"
githubRepository := "pg4scala"
githubTokenSource := TokenSource.Environment("GITHUB_TOKEN") || TokenSource.GitConfig("github.token")
