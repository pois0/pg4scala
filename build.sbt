import sbt.Keys.libraryDependencies
import sbt.url
import xerial.sbt.Sonatype._

//noinspection SpellCheckingInspection
val CommonSettings = Seq(
  organization := "jp.pois",
  licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  //noinspection SpellCheckingInspection
  sonatypeProjectHosting := Some(GitHubHosting("pois0", "pg4scala", "dev@pois.jp")),
  homepage := Some(url("https://github.com/pois0/pg4scala")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/pois0/pg4scala"),
      "scm:git@github.com:pois0/pg4scala.git"
    ),
  ),
  developers := List(
    Developer(id="pois", name="Shusuke Takahashi", email="dev@pois.jp", url=url("https://github.com/pois0"))
  ),

  scalaVersion := "2.13.10",
  idePackagePrefix := Some("jp.pois.pg4scala"),
  version := sys.env.getOrElse("PG4SCALA_VERSION", "1.0.0-SNAPSHOT")
)

usePgpKeyHex("523C25EDC9F5FAA76A29556261A9DF65B817C971")

lazy val Root = project.in(file("."))
  .settings(CommonSettings)
  .aggregate(Core, Benchmark)

lazy val Core = project.in(file("core"))
  .settings(CommonSettings)
  .settings(
    name := "pg4scala-core",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % Test,
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5" % Compile,
    libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.36" % Compile,

    sonatypeCredentialHost := "s01.oss.sonatype.org",
    sonatypeRepository := "https://s01.oss.sonatype.org/service/local",
    publishTo := sonatypePublishToBundle.value,
  )

lazy val Benchmark = project.in(file("bench"))
  .dependsOn(Core)
  .enablePlugins(JmhPlugin)
  .settings(CommonSettings)
  .settings(
    name := "pg4scala-bench"
  )
