name := "fun scala programming"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.2"

scalariformSettings

libraryDependencies ++= Seq(
  "org.scalaz"        %%  "scalaz-core"   %   "7.1.0",
  "org.scalaz.stream" %%  "scalaz-stream" %   "0.7a",
  "org.specs2"        %%  "specs2"        %   "2.4.2"  %  "test"
)

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"