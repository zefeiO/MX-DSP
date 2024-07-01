scalaVersion := "2.13.12"

scalacOptions ++= Seq(
  "-feature",
  "-language:reflectiveCalls",
)

addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % "5.2.0" cross CrossVersion.full)
libraryDependencies += "org.chipsalliance" %% "chisel" % "5.2.0"
libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "5.0.2"