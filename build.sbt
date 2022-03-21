import sbt.Keys.libraryDependencies
import sbt.url

val CommonSettings = Seq(
  organization := "jp.pois",
  version := sys.env.getOrElse("PG4SCALA_VERSION", "1.0.0-SNAPSHOT"),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/pois0/pg4scala"),
      "scm:git@github.com:pois0/pg4scala.git"
    )
  ),
  developers := List(
    Developer(id="pois0", name="Shusuke Takahashi", email="dev@pois.jp", url=url("https://github.com/pois0"))
  ),
  scalaVersion := "2.12.13",
  idePackagePrefix := Some("jp.pois.pg4scala"),

  githubOwner := "pois0",
  githubRepository := "pg4scala",
  githubTokenSource := TokenSource.Environment("GITHUB_TOKEN") || TokenSource.GitConfig("github.token"),
)

lazy val Root = project.in(file("."))
  .settings(CommonSettings)
  .aggregate(Core, Benchmark)

lazy val Core = project.in(file("core"))
  .enablePlugins(GitHubPackagesPlugin)
  .settings(CommonSettings)
  .settings(
    name := "pg4scala-core",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % Test,
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
  )

lazy val Benchmark = project.in(file("bench"))
  .dependsOn(Core)
  .enablePlugins(JmhPlugin)
  .settings(CommonSettings)
  .settings(
    name := "pg4scala-bench"
  )
