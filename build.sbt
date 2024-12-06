val scala3Version = "3.5.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "mut",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    scalacOptions ++= Seq("-Ximport-suggestion-timeout", "0"),

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
