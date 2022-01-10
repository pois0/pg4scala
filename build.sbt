name := "pg4scala"

version := "0.1"

scalaVersion := "2.12.13"

idePackagePrefix := Some("jp.pois.pg4scala")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4"

githubOwner := "pois0"
githubRepository :="pg4scala"
githubTokenSource := TokenSource.Environment("GITHUB_TOKEN")
