package edu.crea.www

import scala.scalajs._
import scala.scalajs.js.Dynamic.newInstance
import js.annotation.JSExport

import org.scalajs.dom
import dom.document

object WebApp extends js.JSApp {

  private[this] def gexfPath = "data/elastin-abstracts.gexf"

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

  private[this] var maybeGraph : Option[js.Dynamic] = None

  private[this] def resetGraph() : Unit = maybeGraph.map { graph => 

    sigma.graph.clear()
    sigma.graph.read(js.Dynamic.literal(nodes = graph.nodes(), edges = graph.edges()))
    sigma.refresh() 

  } 

  private[this] def viewNeighborhood(id : js.Dynamic) = maybeGraph.map { graph =>

    val neighborhood = graph.neighborhood(id)

    if(neighborhood.edges.length > 1) {

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

  @JSExport 
  def search() = { 
    
    def findLabel = searchInput.map(_.asInstanceOf[js.Dynamic].value)

    def findId(label : js.Dynamic) : Option[js.Dynamic] = maybeGraph.flatMap { graph => Option { 
      graph.nodes().asInstanceOf[js.Array[js.Dynamic]]
        .filter((elem : js.Dynamic) => elem.label == label)
        .map((elem : js.Dynamic) => elem.id)
        .pop
    }}

    for { 
      label <- findLabel
      id <- findId(label)
    } { 
      viewNeighborhood(id)
    } 

  } 

  def main() = {

    sigmajs.renderers.`def` = sigmajs.renderers.canvas

    sigmajs.parsers.gexf(gexfPath, js.Dynamic.literal(), { (gexfSig : js.Dynamic) =>

      maybeGraph = Option(gexfSig.graph)

      resetGraph()

      sigma.bind("doubleClickNode", (e : js.Dynamic) => viewNeighborhood(e.data.node.id))

      sigma.bind("doubleClickStage", (e : js.Dynamic) => resetGraph())

    })

  }

}
