ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "2.13.6"

enablePlugins(EnergyMonitorPlugin)

lazy val root = (project in file(".")).settings(
  name := "droste-playground",
  libraryDependencies ++= Seq(
    // standard "effect" library (Queues, Console, Random etc.)
    "org.typelevel" %% "cats-effect-std" % "3.3.1",
    "io.higherkindness" %% "droste-core" % "0.9.0-M3",
    // better monadic for compiler plugin as suggested by documentation
    compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test
  )
)
