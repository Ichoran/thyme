// Build file copyright (c) 2016 by Rex Kerr, HHMI Janelia, UCSF and Calico
// Life Sciences.

/////////////////////////////////
// Sonatype publishing section //
/////////////////////////////////

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomExtra := (
  <url>http://www.github.com/ichoran</url>
  <licenses>
    <license>
      <name>BSD 3-clause</name>
      <url>https://opensource.org/licenses/BSD-3-Clause</url>
      <distribution>repo</distribution>
      <comments>Copyright 2013, 2016 by Rex Kerr, HHMI Janelia, UC San Francisco, and Calico Life Sciences.</comments>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:ichoran/thyme.git</url>
    <connection>scm:git:git@github.com:ichoran/thyme.git</connection>
  </scm>
  <developers>
    <developer><id>ichoran</id><name>Rex Kerr</name></developer>
  </developers>
)


////////////////////////
// Main build section //
////////////////////////

lazy val thyme = (project in file(".")).settings(
  name := "thyme",
  version := "0.1.2-SNAPSHOT",
  scalaVersion := "2.12.0",
  organization := "com.github.ichoran",
  unmanagedSourceDirectories in Compile := {val b = baseDirectory.value; Seq(b / "maths", b / "bench")}
)
