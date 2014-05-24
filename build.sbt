name := "thyme"

version := "0.1.1"

scalaVersion := "2.11.0"

unmanagedSourceDirectories in Compile := {val b = baseDirectory.value; Seq(b / "maths", b / "bench")}

