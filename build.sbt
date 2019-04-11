// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val projectVersion = "0.2.1-SNAPSHOT"
lazy val mimaVersion    = "0.2.0"

lazy val commonSettings = Seq(
  name               := "Numbers",
  version            := projectVersion,
  organization       := "de.sciss",
  scalaVersion       := "2.12.8",
  crossScalaVersions := Seq("2.12.8", "2.11.12", "2.13.0-RC1"),
  description        := "A collection of numeric functions and type enrichments",
  homepage           := Some(url(s"https://github.com/Sciss/${name.value}")),
  licenses           := Seq("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt")),
  mimaPreviousArtifacts := Set("de.sciss" %% "numbers" % mimaVersion),
  initialCommands in console := """import de.sciss.numbers.Implicits._""",
  libraryDependencies += {
    "org.scalatest" %%% "scalatest" % "3.1.0-SNAP9" % Test
  },
  scalacOptions ++= Seq("-Xlint", "-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xlint", "-Xsource:2.13")
)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishTo := {
    Some(if (isSnapshot.value)
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
    )
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  pomExtra := { val n = name.value
<scm>
  <url>git@github.com:Sciss/{n}.git</url>
  <connection>scm:git:git@github.com:Sciss/{n}.git</connection>
</scm>
<developers>
  <developer>
    <id>sciss</id>
    <name>Hanns Holger Rutz</name>
    <url>http://www.sciss.de</url>
  </developer>
</developers>
}
)

lazy val root = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("."))
  .settings(commonSettings)
  .settings(publishSettings)

