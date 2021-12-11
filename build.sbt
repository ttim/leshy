import sbt.Keys.libraryDependencies

ThisBuild / scalaVersion := "2.13.7"
crossScalaVersions := Seq("2.13.7", "3.1.0")

enablePlugins(JmhPlugin)

lazy val root = (project in file("."))
  .settings(
    name := "root",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "org.ow2.asm" % "asm" % "9.2",
    libraryDependencies += "org.ow2.asm" % "asm-util" % "9.2",
    libraryDependencies += "org.ow2.asm" % "asm-tree" % "9.2",
    javaOptions ++= Seq(
      "--add-modules", "jdk.incubator.foreign"
    ),
    fork := true
  )
