name := "pg4scala"

version := "0.1"

scalaVersion := "2.12.13"

idePackagePrefix := Some("jp.pois.pg4scala")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test

//scalacOptions in Global += "-language:experimental.macros"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
