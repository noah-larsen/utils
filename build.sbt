name := "alternacat"

version := "0.1"

scalaVersion := "2.12.4"

val circeVersion = "0.9.3"

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.4" % "test"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.9"
libraryDependencies += "org.rogach" %% "scallop" % "3.1.3"
libraryDependencies += "org.apache.lucene" % "lucene-core" % "7.4.0"
libraryDependencies += "org.apache.lucene" % "lucene-queryparser" % "7.4.0"
