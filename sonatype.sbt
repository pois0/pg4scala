import xerial.sbt.Sonatype._

sonatypeProfileName := "jp.pois"
publishMavenStyle := true
licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
//noinspection SpellCheckingInspection
sonatypeProjectHosting := Some(GitHubHosting("pois0", "pg4scala", "dev@pois.jp"))
homepage := Some(url("https://github.com/pois0/pg4scala"))
scmInfo := Some(
  ScmInfo(
    url("https://github.com/pois0/pg4scala"),
    "scm:git@github.com:pois0/pg4scala.git"
  )
)
developers := List(
  Developer(id="pois", name="Shusuke Takahashi", email="dev@pois.jp", url=url("https://github.com/pois0"))
)
version := sys.env.getOrElse("PG4SCALA_VERSION", "1.0.0-SNAPSHOT")
sonatypeCredentialHost := "s01.oss.sonatype.org"
sonatypeRepository := "https://s01.oss.sonatype.org/service/local"
