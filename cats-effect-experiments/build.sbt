name := "cats-effect-experiments"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.7"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",   // source files are in UTF-8
  "-deprecation",         // warn about use of deprecated APIs
  "-unchecked",           // warn about unchecked type parameters
  "-feature",             // warn about misused language features
  "-language:higherKinds",// allow higher kinded types without `import scala.language.higherKinds`
  "-Xlint",               // enable handy linter warnings
  // "-Xfatal-warnings",     // turn compiler warnings into errors
  "-Ypartial-unification", // allow the compiler to unify type constructors of different arities
  "-language:postfixOps"   
)

libraryDependencies += "org.typelevel" %% "cats-core" % "1.5.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.1.0"
// available for Scala 2.11, 2.12
libraryDependencies += "co.fs2" %% "fs2-core" % "1.0.0" // For cats 1.4.0 and cats-effect 1.0

// optional I/O library
libraryDependencies += "co.fs2" %% "fs2-io" % "1.0.0"

// optional reactive streams interop
libraryDependencies += "co.fs2" %% "fs2-reactive-streams" % "1.0.0"

// optional experimental library
libraryDependencies += "co.fs2" %% "fs2-experimental" % "1.0.0"
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
