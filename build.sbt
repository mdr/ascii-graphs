import com.typesafe.sbt.SbtScalariform._
name := "ascii-graphs"

organization := "com.github.mdr"

version := "0.0.7"

scalaVersion := "2.10.6"

crossScalaVersions := Seq("2.10.6")

scalacOptions ++= Seq("-deprecation", "-Xlint")

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

EclipseKeys.withSource := true

EclipseKeys.eclipseOutput := Some("bin")

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

// scalacOptions in (Compile, doc) += "-diagrams"
