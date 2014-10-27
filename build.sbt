name := "fun scala programming"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.2"

scalariformSettings

libraryDependencies ++= Seq(
  "org.specs2"   %%  "specs2"   %   "2.4.2"    %  "test"
)

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)