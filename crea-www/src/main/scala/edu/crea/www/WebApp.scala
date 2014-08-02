package edu.crea.www

import scala.util.matching.Regex
import scala.util.{Try, Success, Failure}

import scala.scalajs._
import scala.scalajs.js.Dynamic.newInstance
import js.annotation.JSExport

import org.scalajs.dom
import dom.document

object WebApp extends js.JSApp {

  private[this] def gexfPath : String = "data/elastin-abstracts.gexf"

  private[this] def autocomplete(input : String) : Regex = input.toList.mkString("""[\s\w]*""").concat("""[\s\w]*""").r

  private[this] def searchInput : Option[dom.HTMLElement] = {
    Option(document.getElementById("search-input"))
  }

  private[this] def sigmajs = js.Dynamic.global.sigma

  private[this] def settings = js.Dynamic.literal(
    font = "serif",
    drawEdgeLabels = true,
    edgeLabelSize = "fixed",
    defaultEdgeLabelSize = 12,
    defaultEdgeLabelColor = "#997F46",
    edgeLabelThreshold = 1.0,
    labelSize = "proportional",
    labelThreshold = 2.5,
    labelSizeRatio = 3.0,
    defaultLabelSize = 32,
    defaultLabelColor = "#FFD67D",
    minNodeSize = 0.5,
    maxNodeSize = 5.0,
    minEdgeSize = 0.1,
    maxEdgeSize = 0.4,
    scalingMode = "outside",
    hideEdgesOnMove = true,
    doubleClickEnabled = false
  )

  private[this] def config = js.Dynamic.literal(
    container = "graph-container",
    graph = newInstance(sigmajs.classes.graph)(),
    settings = settings
  )

  private[this] lazy val sigma = newInstance(sigmajs)(config)

  private[this] def dictionary : Array[String] = {
    sigma.graph.nodes().asInstanceOf[js.Array[js.Dynamic]].map((_ : js.Dynamic).label.asInstanceOf[String])
  }

  private[this] var findGraph : Option[js.Dynamic] = None

  private[this] def resetGraph() : Unit = findGraph.map { graph =>

    sigma.graph.clear()
    sigma.graph.read(js.Dynamic.literal(nodes = graph.nodes(), edges = graph.edges()))
    sigma.refresh()

  }

  private[this] def findNeighborhood(id : js.Dynamic) : Option[js.Dynamic] = findGraph.flatMap { graph =>

    Try(graph.neighborhood(id)) match {

      case Success(neighborhood) =>

        if(neighborhood.nodes.length > 1) {
          Some(neighborhood)
        } else {
          None
        }

      case Failure(_) => None

    }

  }


  private[this] def viewNeighborhood(id : js.Dynamic) = findGraph.map { graph =>

    for(neighborhood <- findNeighborhood(id)) {

      sigma.graph.clear()
      sigma.graph.read(neighborhood)
      sigma.camera.goTo(js.Dynamic.literal(
        x = 0,
        y = 0,
        angle = 0,
        ratio = 1
      ))

      sigma.refresh()

     }

  }

  private[this] def findLabel : Option[String] = searchInput.map(_.asInstanceOf[js.Dynamic].value.asInstanceOf[String])

  private[this] def findId(label : String) : Option[js.Dynamic] = findGraph.flatMap { graph => Option {
    graph.nodes().asInstanceOf[js.Array[js.Dynamic]]
      .filter((elem : js.Dynamic) => elem.label.asInstanceOf[String] == label)
      .map((elem : js.Dynamic) => elem.id)
      .pop
  }}

  @JSExport
  def search() = for {
    label <- findLabel
    id <- findId(label)
  } {
    viewNeighborhood(id)
  }

  @JSExport
  def showSuggestions() = Option(document.getElementById("suggestions")).map { suggestions =>

    while(suggestions.hasChildNodes()) {
      suggestions.removeChild(suggestions.lastChild)
    }

    for {
      label <- findLabel
    } {

      val dict = dictionary

      if(dict.size <= 500) {

        val items = dictionary.filter(suggestion => autocomplete(label).pattern.matcher(suggestion).matches)
          .sortBy(Levenshtein(label))
          .take(20)

        if(items.length > 0) {

          items.foreach { suggestion =>

            val li = document.createElement("li")
            li.onclick = ( e : dom.MouseEvent) => findId(suggestion).foreach(id => viewNeighborhood(id))
            li.appendChild(document.createTextNode(suggestion))

            suggestions.appendChild(li)

          }

        suggestions.style.display = "block";

        } else {

          hideSuggestions();

        }

      }

    }

  }

  @JSExport
  def hideSuggestions() = {

    document.getElementById("suggestions").style.display = "none";

  }

  def main() = {

    sigmajs.renderers.`def` = sigmajs.renderers.canvas

    sigmajs.parsers.gexf(gexfPath, js.Dynamic.literal(), { (gexfSig : js.Dynamic) =>

      findGraph = Option(gexfSig.graph)

      resetGraph()

      sigma.bind("doubleClickNode", (e : js.Dynamic) => viewNeighborhood(e.data.node.id))

      sigma.bind("doubleClickStage", (e : js.Dynamic) => resetGraph())

    })

  }

}
