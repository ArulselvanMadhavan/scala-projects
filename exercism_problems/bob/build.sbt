scalaVersion := "2.12.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

addCommandAlias("fmt", ";scalafmt ;test:scalafmt ;it:scalafmt")
