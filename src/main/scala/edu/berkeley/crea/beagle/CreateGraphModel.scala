package edu.berkeley.crea.beagle

import org.gephi.graph.api.GraphModel

object CreateGraphModel {

  def apply() : GraphModel = {

    import org.gephi.project.api.ProjectController
    import org.gephi.project.api.Workspace
    import org.gephi.graph.dhns.DhnsGraphController
    import org.openide.util.Lookup

    val pc = Lookup.getDefault.lookup(classOf[ProjectController])
    pc.newProject()
    val workspace = pc.getCurrentWorkspace
    val controller = new DhnsGraphController

    controller.newDhns(workspace)

  }

}
