package edu.crea.www

import scala.scalajs._
import scala.scalajs.js.Dynamic.{newInstance => jsNew}

object WebApp extends js.JSApp {

  def main() = {

    def gexfPath = "data/elastin-abstracts.gexf"

    def sigmajs = js.Dynamic.global.sigma

    def settings = js.Dynamic.literal(
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

    sigmajs.renderers.`def` = sigmajs.renderers.canvas

    val sigma = jsNew(sigmajs)(js.Dynamic.literal(
      container = "graph-container",
      graph = jsNew(sigmajs.classes.graph)(),
      settings = settings
    ))

    sigmajs.parsers.gexf(gexfPath, js.Dynamic.literal(), { (self : js.Object, gexfSig : js.Dynamic) =>

      val graph = js.Dynamic.literal(
        nodes = gexfSig.graph.nodes()
        edges = gexfSig.graph.edges()
      )

      println(js.Dynamic.global.JSON.stringify(gexfSig))

      sigma.graph.clear()
      sigma.graph.read(graph)
      sigma.refresh()

      println("ok")

      sigma.bind("doubleClickNode", { (self : js.Object, e : js.Dynamic) =>

        val neighborhood = gexfSig.graph.neighborhood(e.data.node.id)

        if(neighborhood.edges.length > 1) {

          sigma.graph.clear()
          sigma.graph.read(neighborhood)
          sigma.camera.goto(js.Dynamic.literal(
            x = 0,
            y = 0,
            angle = 0,
            ratio = 1
          ))

        }

      } : js.ThisFunction)

      sigma.bind("doubleClickStage", { (self : js.Object, e : js.Dynamic) =>

        sigma.graph.clear()
        sigma.graph.read(graph)
        sigma.refresh()

      } : js.ThisFunction)

    } : js.ThisFunction)

  }

}
