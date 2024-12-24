ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(name := "advent-of-code-2024")
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.1.0"