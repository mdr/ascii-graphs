name := "ascii-graphs"

organization := "com.github.mdr"

version := "0.0.3"

scalaVersion := "2.10.1"

scalacOptions ++= Seq("-deprecation","-feature","-optimize")

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

EclipseKeys.withSource := true

EclipseKeys.eclipseOutput := Some("bin")

net.virtualvoid.sbt.graph.Plugin.graphSettings

publishMavenStyle := true

publishArtifact in Test := false

publishTo <<= isSnapshot(
  if (_) Some("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/") 
  else   Some("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2/"))

pomExtra := {
    <inceptionYear>2012</inceptionYear>
    <url>http://github.com/mdr/ascii-graphs</url>
    <licenses>
      <license>
        <name>MIT License</name>
        <url>http://www.opensource.org/licenses/mit-license.php</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:mdr/ascii-graphs.git</url>
      <connection>scm:git:git@github.com:mdr/ascii-graphs</connection>
    </scm>
    <developers>
      <developer>
        <id>mdr</id>
        <name>Matt Russell</name>
        <url>https://github.com/mdr/</url>
      </developer>
    </developers>
  }
