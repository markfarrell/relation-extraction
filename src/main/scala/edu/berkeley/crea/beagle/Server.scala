package edu.berkeley.crea.beagle

import java.io.File


import org.gephi.streaming.server.StreamingServer
import org.gephi.streaming.server.impl.ServerControllerImpl
import org.gephi.streaming.server.impl.jetty.StreamingServerImpl

import org.gephi.graph.api.GraphModel

import akka.actor._

object Server {

  def apply(model : GraphModel) = {

    val graph = model.getGraph

    val server = new StreamingServerImpl
    val controller = new ServerControllerImpl(graph)

    server.register(controller, "/streaming")
    server.start()

  }

  def main(args : Array[String]) : Unit = {

    val parser = new MemoizedParser
    val model = CreateGraphModel()
    val compiler = new Compiler(model)

    Server(model)
    parser.read(System.in, compiler.apply)


  }

}


