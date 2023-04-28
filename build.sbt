ThisBuild / scalaVersion     := "3.2.2"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "io.navidjalali"
ThisBuild / organizationName := "navidjalali"

lazy val root = (project in file("."))
  .settings(
    name := "Playground",
    libraryDependencies ++= Seq(),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
