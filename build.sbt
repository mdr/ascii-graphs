name := "ascii-graphs"

organization := "com.github.mdr"

version := "0.0.7-SNAPSHOT"

scalaVersion := "2.12.3"

crossScalaVersions := Seq("2.10.6", "2.11.11", "2.12.3")

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked"
)


libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % Test,
  "org.scalacheck" %% "scalacheck" % "1.13.4" % Test
)

// Screen-sized dependency graph:
// libraryDependencies += "org.vert-x" % "vertx-core" % "1.3.1.final"

EclipseKeys.withSource := true

EclipseKeys.eclipseOutput := Some("bin")

//net.virtualvoid.sbt.graph.Plugin.graphSettings

import com.typesafe.sbt.SbtScalariform.ScalariformKeys

ScalariformKeys.preferences := {
  val dir = baseDirectory.value
  scalariform.formatter.preferences.PreferencesImporterExporter.loadPreferences((dir / "formatterPreferences.properties").getPath)
}

publishMavenStyle := true

publishArtifact in Test := false

publishTo :=  {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT")) {
    Some("snapshots" at nexus + "content/repositories/snapshots")
  }
  else {
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }
}

startYear := Some(2012)

homepage := Some(url("http://github.com/mdr/ascii-graphs"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/mdr/ascii-graphs"),
    "scm:git:git@github.com:mdr/ascii-graphs.git"
  )
)

developers +=
  Developer(
    "mdr",
    "Matt Russell",
    "MattRussellUK@gmail.com",
    url("https://github.com/mdr/")
  )

licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php"))

// scalacOptions in (Compile, doc) += "-diagrams"
