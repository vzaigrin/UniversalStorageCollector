assemblyJarName in assembly := "collector.jar"

name := "UniversalStorageCollector"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += "JAnalyse Repository" at "http://www.janalyse.fr/repository/"

libraryDependencies ++= Seq(
  "org.scala-lang.modules"	%  "scala-xml_2.11"	% "1.0.6",
  "com.typesafe.akka"		%% "akka-actor"		% "2.4.14",
  "fr.janalyse"			%% "janalyse-ssh"	% "0.9.19"	% "compile",
  "org.influxdb"		%  "influxdb-java"	% "2.5",
  "org.slf4j"			%  "slf4j-nop"		% "1.6.4",
  "net.liftweb"			%  "lift-json_2.11"	% "3.1.0-M1",
  "org.scalaj"			%% "scalaj-http"	% "2.3.0"
)
