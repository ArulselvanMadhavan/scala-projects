import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      scalaOrganization := "org.typelevel",
      scalaVersion := "2.12.4-bin-typelevel-4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Algorithms",
    libraryDependencies ++= Seq(
      scalaTest % Test,
        "com.chuusai" %% "shapeless" % "2.3.3"
    )
  )
