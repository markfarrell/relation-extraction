scalaJSSettings

workbenchSettings

name := "crea-www"

scalaVersion := "2.11.1"

bootSnippet := "edu.crea.www.WebApp.main();"

libraryDependencies += "org.scala-lang.modules.scalajs" %%% "scalajs-dom" % "0.6"

ScalaJSKeys.jsDependencies += ProvidedJS / "sigma.min.js"

ScalaJSKeys.jsDependencies += ProvidedJS / "sigma.parsers.gexf.min.js" dependsOn "sigma.min.js"

ScalaJSKeys.jsDependencies += ProvidedJS / "sigma.plugins.dragNodes.min.js" dependsOn "sigma.min.js"

ScalaJSKeys.jsDependencies += ProvidedJS / "sigma.plugins.neighborhoods.min.js" dependsOn "sigma.min.js"

ScalaJSKeys.preLinkJSEnv := new scala.scalajs.sbtplugin.env.phantomjs.PhantomJSEnv

skip in ScalaJSKeys.packageJSDependencies := false
