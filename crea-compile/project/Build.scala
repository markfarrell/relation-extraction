import com.typesafe.sbt.SbtNativePackager._
import NativePackagerKeys._
import sbt._
import Keys._

object ProjectBuild extends Build {

  def standardSettings = Defaults.defaultSettings ++ Seq(
    initialCommands in console := """import crea.nlp._
      import scalaz._
      import Scalaz._
      import Patterns._
      import Terms._
      import Trees._
    """,
    name := "crea-compile",
    version := "1.0",
    scalaVersion := "2.11.1",
    fork in run := true,
    javaOptions += "-Xmx8G -Xss128M -Xms1G -XX:SurvivorRatio=16 -XX:NewRatio=5 -XX:+UseConcMarkSweepGC",
    maintainer in Debian := "Mark Farrell",
    mainClass in (Compile) := Some("crea.nlp.GexfCompiler"),
    packageDescription in Debian := "Compiles text into text-networks.",
    resolvers ++= Seq(
      "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases",
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
      "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.0" % "test",
      "org.slf4j" % "slf4j-api" % "1.7.7",
      "org.slf4j" % "slf4j-log4j12" % "1.7.7",
      "log4j" % "log4j" % "1.2.17",
      "it.uniroma1.dis.wsngroup.gexf4j" % "gexf4j" % "0.4.4-BETA",
      "com.github.scopt" %% "scopt" % "3.2.0",
      "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1",
      "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1" classifier "models",
      "org.scalaz" %% "scalaz-core" % "7.0.6",
      "org.scalaz.stream" %% "scalaz-stream" % "0.4.1",
      "com.chuusai" %% "shapeless" % "2.0.0",
      "com.bizo" % "mighty-csv_2.11" % "0.2",
      "net.sourceforge.owlapi" % "owlapi-distribution" % "3.4.10",
      "net.sf.jwordnet" % "jwnl" % "1.4_rc3"
    )
  )

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = packageArchetype.java_application ++ standardSettings
  )

}

