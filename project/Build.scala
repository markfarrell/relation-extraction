import com.typesafe.sbt.SbtStartScript
import sbt._
import Keys._

object ProjectBuild extends Build {

  def standardSettings = Defaults.defaultSettings ++ Seq(
    exportJars := true,
    name := "beagle",
    version := "0.1",
    scalaVersion := "2.10.3",
    resolvers ++= Seq(
      "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases",
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
    ),
    libraryDependencies ++= Seq(
      "javax.servlet" % "javax.servlet-api" % "3.1.0",
      "org.eclipse.jetty" % "jetty-continuation" % "8.1.3.v20120416",
      "org.eclipse.jetty" % "jetty-http" % "8.1.3.v20120416",
      "org.eclipse.jetty" % "jetty-io" % "8.1.3.v20120416",
      "org.eclipse.jetty" % "jetty-security" % "8.1.3.v20120416",
      "org.eclipse.jetty" % "jetty-server" % "8.1.3.v20120416",
      "org.eclipse.jetty" % "jetty-servlet" % "8.1.3.v20120416",
      "org.eclipse.jetty" % "jetty-util" % "8.1.3.v20120416",
      "org.eclipse.jetty" % "jetty-websocket" % "8.1.3.v20120416",
      "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
      "org.slf4j" % "slf4j-api" % "1.7.7",
      "org.slf4j" % "slf4j-log4j12" % "1.7.7",
      "log4j" % "log4j" % "1.2.17",
      "it.uniroma1.dis.wsngroup.gexf4j" % "gexf4j" % "0.4.4-BETA",
      "com.github.scopt" %% "scopt" % "3.2.0",
      "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1",
      "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1" classifier "models",
      "org.mapdb" % "mapdb" % "1.1.0-SNAPSHOT",
      "com.typesafe.akka" %% "akka-actor" % "2.3.3"
    )
  )

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = standardSettings ++ SbtStartScript.startScriptForClassesSettings
  )

}

