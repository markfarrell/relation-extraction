import com.typesafe.sbt.SbtNativePackager._
import NativePackagerKeys._
import sbt._
import Keys._

object ProjectBuild extends Build {

  def standardSettings = Defaults.defaultSettings ++ Seq(
    initialCommands in console := """import edu.crea.nlp._
      import scalaz._
      import Scalaz._
      import Patterns._
      import Terms._
      import Trees._
      lazy val parser = new Parser
      def parse = parser.apply _
      def compile(sentence : String) : Option[Stream[Compound]] = {
        val tree = parse(sentence)
        println(tree.shows)
        RootExpression(tree)
      }
    """,
    name := "crea-compile",
    version := "1.0",
    scalaVersion := "2.11.1",
    maintainer in Debian := "Mark Farrell",
    mainClass in (Compile) := Some("edu.crea.gexf.GexfCompiler"),
    packageDescription in Debian := "Compiles textbooks into text-networks.",
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
      "com.chuusai" %% "shapeless" % "2.0.0"
    )
  )

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = packageArchetype.java_application ++ standardSettings
  )

}

