name := "ascii-graphs"

organization := "com.github.mdr"

version := "0.0.7-SNAPSHOT"

scalaVersion := "2.12.6"

//crossScalaVersions := Seq("2.10.6", "2.11.11", "2.12.3")

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

startYear := Some(2012)

homepage := Some(url("http://github.com/mdr/ascii-graphs"))

developers +=
  Developer(
    "mdr",
    "Matt Russell",
    "MattRussellUK@gmail.com",
    url("https://github.com/mdr/")
  )

licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php"))
