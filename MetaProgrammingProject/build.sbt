name := "MetaProgrammingProject"

version := "0.1"

scalaVersion := "2.12.7"

lazy val root = (project in file(".")).dependsOn(PartII)

lazy val PartII = RootProject(uri("https://github.com/Al153/PartIIProject.git"))