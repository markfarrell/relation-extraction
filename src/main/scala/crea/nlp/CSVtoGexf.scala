
import com.github.tototoshi.csv._
import it.uniroma1.dis.wsngroup.gexf4j.core._
import it.uniroma1.dis.wsngroup.gexf4j.core.data._
import it.uniroma1.dis.wsngroup.gexf4j.core.impl._
import it.uniroma1.dis.wsngroup.gexf4j.core.impl.data._
import it.uniroma1.dis.wsngroup.gexf4j.core.dynamic._
import java.io.FileOutputStream
import scala.collection.mutable.HashMap

object CSVtoGexf {

  def apply(csvFile : String) : Unit = {

    val nodeTable = HashMap.empty[String, Node]

    val charset = "UTF-8"

    val fs = new FileOutputStream(s"${System.currentTimeMillis}.gexf")

    val reader = CSVReader.open(csvFile)

    val gexf = new GexfImpl
    val graph = gexf.getGraph
    graph.setDefaultEdgeType(EdgeType.DIRECTED)
    graph.setMode(Mode.DYNAMIC)
    graph.setTimeType(TimeFormat.INTEGER)

    val attrList = new AttributeListImpl(AttributeClass.EDGE)
    graph.getAttributeLists.add(attrList)

    val attrPMID = attrList.createAttribute("0", AttributeType.STRING, "PMID")
    val attrTerm = attrList.createAttribute("1", AttributeType.STRING, "term")

    val writer = new StaxGraphWriter

    reader.foreach { _ match {
      case List(pmid : String, predicate : String, subject : String, obj : String, term : String, _ : String, _ : String) =>

      val sourceNode = nodeTable.get(subject).getOrElse {
        graph.createNode(subject).setLabel(subject)
      }

      val targetNode = nodeTable.get(obj).getOrElse {
        graph.createNode(obj).setLabel(obj)
      }

      val edge = sourceNode.connectTo(targetNode)

      edge.setLabel(predicate)
      edge.getAttributeValues.addValue(attrPMID, pmid)
      edge.getAttributeValues.addValue(attrTerm, term)

      case other => println(other)

    } }

    writer.writeToStream(gexf, fs, charset)

    reader.close()
    fs.close()

  }

}
