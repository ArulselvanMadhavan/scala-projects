import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.arulselvan",
      scalaVersion := "2.12.7",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "FP-for-mortals",
    libraryDependencies ++= Seq(
      scalaz,
      simulacrum,
      refined,
      scalaTest % Test
    ),
    scalacOptions in ThisBuild ++= Seq(
      "-language:_",
      "-Ypartial-unification",
      "-Xfatal-warnings"
    ),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7"),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
  )
initialCommands in console := "import scalaz._, Scalaz._"
