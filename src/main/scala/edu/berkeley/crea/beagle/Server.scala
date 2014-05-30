package edu.berkeley.crea.beagle

import org.gephi.streaming.server.StreamingServer
import org.gephi.streaming.server.impl.ServerControllerImpl
import org.gephi.streaming.server.impl.jetty.StreamingServerImpl

import org.gephi.graph.api.GraphModel

object Server {

  def apply(model : GraphModel) = {

    val graph = model.getGraph

    val server = new StreamingServerImpl
    val controller = new ServerControllerImpl(graph)

    server.register(controller, "/streaming")
    server.start()

  }

  def main(args : Array[String]) : Unit = Server(CreateGraphModel())

}
