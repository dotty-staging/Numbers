lazy val projectVersion = "0.2.1-SNAPSHOT"
lazy val mimaVersion    = "0.2.0"

lazy val deps = new {
  val test = new {
    val scalaTest = "3.2.2"
  }
}

lazy val commonSettings = Seq(
  name               := "Numbers",
  version            := projectVersion,
  organization       := "de.sciss",
  scalaVersion       := "2.13.3",
  crossScalaVersions := Seq("0.27.0-RC1", "2.13.3", "2.12.12"),
  description        := "A collection of numeric functions and type enrichments",
  homepage           := Some(url(s"https://git.iem.at/sciss/${name.value}")),
  licenses           := Seq("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt")),
) ++ publishSettings

lazy val root = project.in(file("."))
  .settings(commonSettings)
  .settings(
    mimaPreviousArtifacts := Set("de.sciss" %% "numbers" % mimaVersion),
    initialCommands in console := """import de.sciss.numbers.Implicits._""",
    libraryDependencies += "org.scalatest" %% "scalatest" % deps.test.scalaTest % Test,
    scalacOptions ++= Seq("-Xlint", "-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xsource:2.13"),
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
  <url>git@git.iem.at:sciss/{n}.git</url>
  <connection>scm:git:git@git.iem.at:sciss/{n}.git</connection>
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

