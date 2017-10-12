lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "brainysmith",
      scalaVersion := "2.12.1",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Radix-Tree",
    scalacOptions ++= Seq("-deprecation"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test
  )
