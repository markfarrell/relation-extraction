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
      "gephi-nexus" at "http://nexus.gephi.org/nexus/content/repositories/snapshots"
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
      "org.slf4j" % "slf4j-simple" % "1.7.5",
      "it.uniroma1.dis.wsngroup.gexf4j" % "gexf4j" % "0.4.4-BETA",
      "postgresql" % "postgresql" % "8.4-701.jdbc4",
      "com.github.scopt" %% "scopt" % "3.2.0",
      "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1",
      "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1" classifier "models",
      "org.gephi" % "graphstore" % "0.1-SNAPSHOT"
    )
  )

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = standardSettings ++ SbtStartScript.startScriptForClassesSettings
  )

}

