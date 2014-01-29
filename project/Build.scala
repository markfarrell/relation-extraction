import com.typesafe.sbt.SbtStartScript
import sbt._
import Keys._

object ProjectBuild extends Build {

  def standardSettings = Defaults.defaultSettings ++ Seq( 
    exportJars := true,
    name := "berkeley-lazy-cache",

    version := "0.1",

    scalaVersion := "2.10.3",

    libraryDependencies ++= Seq(
      "org.slf4j" % "slf4j-simple" % "1.7.5",
      "it.uniroma1.dis.wsngroup.gexf4j" % "gexf4j" % "0.4.4-BETA"
    )
  ) 

  lazy val scala_redis = RootProject(uri("https://github.com/debasishg/scala-redis.git"))

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = standardSettings ++ SbtStartScript.startScriptForClassesSettings
  ) dependsOn(scala_redis) 

} 

