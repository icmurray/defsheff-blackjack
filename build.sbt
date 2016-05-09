version in ThisBuild := "0.1.0"

scalaVersion in ThisBuild := "2.11.8"

scalacOptions in ThisBuild ++= Seq(
  "-feature",
  "-deprecation",
  "-Xlint",
  "-unchecked")
  //"-Ywarn-unused-import")

// Some dependencies that should be shared across all sub-projects
libraryDependencies in ThisBuild += "org.specs2"     %% "specs2-core" % "3.7.2" % "test"
libraryDependencies in ThisBuild += "org.specs2"     %% "specs2-scalacheck" % "3.7.2" % "test"
//libraryDependencies in ThisBuild += "org.typelevel"  %% "scalaz-specs2" % "0.4.0" % "test"
//libraryDependencies in ThisBuild += "org.scalaz"     %% "scalaz-scalacheck-binding" % "7.1.4" % "test"
//libraryDependencies in ThisBuild += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
//libraryDependencies in ThisBuild += "com.typesafe"    % "config" % "1.3.0"
//libraryDependencies in ThisBuild += "com.github.melrief" %% "pureconfig" % "0.1.5"
//libraryDependencies in ThisBuild += "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
//libraryDependencies in ThisBuild += "ch.qos.logback" % "logback-classic" % "1.1.5"
//
//
//// For specs2
//scalacOptions in Test in ThisBuild ++= Seq("-Yrangepos")

lazy val main = (project in file("main")).settings(
  libraryDependencies ++= Seq(

    "org.scalaz" %% "scalaz-core" % "7.2.2",
    "org.scalaz" %% "scalaz-concurrent" % "7.2.2",

    "com.github.julien-truffaut"  %%  "monocle-core"    % "1.2.0",
    "com.github.julien-truffaut"  %%  "monocle-generic" % "1.2.0",
    "com.github.julien-truffaut"  %%  "monocle-macro"   % "1.2.0"
  )
)
