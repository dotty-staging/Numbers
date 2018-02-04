lazy val mimaVersion = "0.1.3"

name               := "Numbers"
version            := "0.1.4"
organization       := "de.sciss"
scalaVersion       := "2.12.4"
crossScalaVersions := Seq("2.12.4", "2.11.12")
description        := "A collection of numeric functions and type enrichments"
homepage           := Some(url(s"https://github.com/Sciss/${name.value}"))
licenses           := Seq("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt"))

mimaPreviousArtifacts := Set("de.sciss" %% "numbers" % mimaVersion)

initialCommands in console := """import de.sciss.numbers.Implicits._"""

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

scalacOptions ++= Seq("-Xlint", "-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture")

// ---- publishing ----

publishMavenStyle := true

publishTo :=
  Some(if (isSnapshot.value)
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  else
    "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
  )

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

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
