package edu.crea.www

import scala.scalajs._
import scala.scalajs.js.Dynamic.newInstance
import js.annotation.JSExport

import org.scalajs.dom
import dom.document

object WebApp extends js.JSApp {

  private[this] def gexfPath = "data/elastin-abstracts.gexf"

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

  @JSExport 
  def search = println("Searching")

  def main() = {

    sigmajs.renderers.`def` = sigmajs.renderers.canvas

    sigmajs.parsers.gexf(gexfPath, js.Dynamic.literal(), { (gexfSig : js.Dynamic) =>

      val graph : js.Object = js.Dynamic.literal(
        nodes = gexfSig.graph.nodes(),
        edges = gexfSig.graph.edges()
      )

      sigma.graph.clear()
      sigma.graph.read(graph)
      sigma.refresh()

      sigma.bind("doubleClickNode", { (e : js.Dynamic) =>

        val neighborhood = gexfSig.graph.neighborhood(e.data.node.id)

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

      })

      sigma.bind("doubleClickStage", { (e : js.Dynamic) =>

        sigma.graph.clear()
        sigma.graph.read(graph)
        sigma.refresh()

      })

    })

  }

}
