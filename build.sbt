name := "ascii-graphs"

organization := "com.github.mdr"

version := "0.0.4-SNAPSHOT"

scalaVersion := "2.10.1"

crossScalaVersions := Seq("2.9.1", "2.9.2", "2.10.1")

scalacOptions ++= Seq("-deprecation")

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

// libraryDependencies += "org.antlr" % "antlr" % "3.5"

// libraryDependencies += "org.vert-x" % "vertx-platform" % "1.3.1.final"

//libraryDependencies ++= Seq(
//      "com.typesafe" % "config" % "1.0.0",
//      "org.slf4j" % "slf4j-api" % "1.7.2",
//      "org.slf4j" % "jcl-over-slf4j" % "1.7.2",
//      "org.slf4j" % "log4j-over-slf4j" % "1.7.2",
//      "ch.qos.logback" % "logback-classic" % "1.0.7",
//      "org.codehaus.janino" % "janino" % "2.6.1",
//      "com.fasterxml.jackson.core" % "jackson-databind" % "2.1.0"
//      )


EclipseKeys.withSource := true

EclipseKeys.eclipseOutput := Some("bin")

net.virtualvoid.sbt.graph.Plugin.graphSettings

scalariformSettings

ScalariformKeys.preferences <<= baseDirectory.apply { dir => 
    scalariform.formatter.preferences.PreferencesImporterExporter.loadPreferences((dir / "formatterPreferences.properties").getPath)
}

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
