name := "scala-native-playground"

Global / onChangedBuildSource := ReloadOnSourceChanges

scalaVersion := "2.12.12"
scalacOptions ++= Seq("-feature")
nativeMode := "debug"
nativeGC := "immix"

lazy val hello = (project in file("hello")).enablePlugins(ScalaNativePlugin)
lazy val chapter1 = (project in file("chapter1")).enablePlugins(ScalaNativePlugin)
