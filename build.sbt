lazy val mimaVersion = "0.2.0"

name               := "Numbers"
version            := "0.2.0"
organization       := "de.sciss"
scalaVersion       := "2.12.8"
crossScalaVersions := Seq("2.12.8", "2.11.12", "2.13.0")
description        := "A collection of numeric functions and type enrichments"
homepage           := Some(url(s"https://github.com/Sciss/${name.value}"))
licenses           := Seq("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt"))

mimaPreviousArtifacts := Set("de.sciss" %% "numbers" % mimaVersion)

initialCommands in console := """import de.sciss.numbers.Implicits._"""

libraryDependencies += {
  val v = "3.0.8-RC5"
  if (scalaVersion.value == "2.13.0") {
    "org.scalatest" % "scalatest_2.13.0-RC3" % v % Test
  } else {
    "org.scalatest" %% "scalatest" % v % Test
  }
}

scalacOptions ++= Seq("-Xlint", "-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xsource:2.13")

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
