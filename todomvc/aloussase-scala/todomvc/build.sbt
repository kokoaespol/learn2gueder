import Dependencies._

ThisBuild / scalaVersion     := "2.13.11"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "io.github.aloussase"
ThisBuild / organizationName := "todomvc"

val http4sVersion = "0.23.22"

lazy val root = (project in file("."))
    .aggregate(domain, api)

lazy val domain = (project in file("domain"))
    .settings(
        name := "domain",
        libraryDependencies ++= Seq(
            cats
        )
    )

lazy val dataAccess = (project in file("data_access"))
    .dependsOn(domain % "test->test;compile->compile")
    .settings(
        name := "data_access",
        libraryDependencies ++= Seq(
            cats
        )
    )

lazy val api = (project in file("api")) 
    .dependsOn(
        domain % "test->test;compile->compile",
        dataAccess % "test->test;compile->compile"
    )
    .settings(
        name := "api",
        libraryDependencies ++= Seq(
            cats,
            catsEffect,
            macwire
        ) ++ http4s(http4sVersion)
    )
