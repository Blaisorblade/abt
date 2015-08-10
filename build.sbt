scalaVersion := "2.11.7"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.3"

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test-src"
resourceDirectory in Compile := baseDirectory.value / "resources"
resourceDirectory in Test := baseDirectory.value / "resources"

//Just to stop sbteclipse from creating extra dirs.
javaSource in Compile := (scalaSource in Compile).value
javaSource in Test := (scalaSource in Test).value

EclipseKeys.eclipseOutput in ThisBuild := Some("bin")
