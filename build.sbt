val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc2023",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
        "org.scalameta" %% "munit" % "0.7.29" % Test,
        "org.typelevel" %% "cats-core" % "2.10.0"
        )
    )

