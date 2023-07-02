import sbt._

object Dependencies {
  lazy val munit = "org.scalameta" %% "munit" % "0.7.29"
  lazy val cats = "org.typelevel" %% "cats-core" % "2.9.0"
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "3.5.1"
  lazy val macwire = "com.softwaremill.macwire" %% "macros" % "2.5.8"

  def http4s(http4sVersion: String) = Seq(
    "org.http4s" %% "http4s-ember-client" % http4sVersion,
    "org.http4s" %% "http4s-ember-server" % http4sVersion,
    "org.http4s" %% "http4s-dsl"          % http4sVersion,
    "org.http4s" %% "http4s-circe"        % http4sVersion,
  )
}
