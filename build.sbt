enablePlugins(ScalaJSPlugin)
enablePlugins(ScalaJSJUnitPlugin)

name := "Coderover"

scalaVersion := "2.11.12" // or any other Scala version >= 2.11.12

libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_sjs1_2.11" % "2.1.1"

// libraryDependencies += "org.wvlet.airframe" %%% "airspec" % "20.6.0" % "test"
// testFrameworks += new TestFramework("wvlet.airspec.Framework")

// This is an application with a main method
//scalaJSUseMainModuleInitializer := true