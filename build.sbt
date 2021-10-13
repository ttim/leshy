import sbt.Keys.libraryDependencies

ThisBuild / scalaVersion := "3.0.2"

enablePlugins(JmhPlugin)

lazy val root = (project in file("."))
  .settings(
    name := "root",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    javaOptions ++= Seq(
      "--add-modules", "jdk.incubator.foreign"
    ),
    fork := true
  )
