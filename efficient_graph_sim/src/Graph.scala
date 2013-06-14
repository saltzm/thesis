import scala.io._
import java.io._

/** A class representing a Graph 
  */
case class Graph(adjList: Array[Set[Int]], 
                 labels: Array[Int], 
                 labelMap: Map[Int, Set[Int]]) {

  def size = adjList.size
  def nEdges = adjList.foldLeft(0) { (n, i) => n + i.size }

  def print {
    println("adjList: ")
    adjList.foreach { println(_) }
    println("labels: ")
    labels.foldLeft(0) {(i, l) => { println(i + ": "+ l); i + 1; }}
    println("labelMap: ")
    labelMap.foreach{ case(k, v) => println(k + ": " + v) }
  }

  def writeToFile(filename: String) {
    val out = new PrintWriter(filename)
    for (i <- adjList.indices) {
      out.println(i + " " + labels(i) + " " + adjList(i).foldLeft("")(
        (str, n) => n + " " + str
      ))
    } 
    out.close
  }

  def writeToFileNeo4J(nodeFile: String, edgeFile: String) {
    val nodeOut = new PrintWriter(nodeFile)
    nodeOut.println("id\tlabel")
    labels.foldLeft(1) {(i, l) => nodeOut.println(i+"\t"+l); i+1; }
    nodeOut.close
    val edgeOut = new PrintWriter(edgeFile)
    edgeOut.println("start\tend\ttype")
    adjList.foldLeft(1) { (i, n) =>
      n.foreach { c => edgeOut.println(i + "\t" + (c+1) + "\tEDGE") }
      i + 1
    }
    edgeOut.close
  }
}

object Graph {
  /**
    * Read a graph from a file
    */
  def apply(labelFile: String, edgeFile: String): Graph = {
    val adjList = Source.fromFile(edgeFile).getLines.map(
      line =>
        if(!(line.trim equals ""))
          line.split(" ").map( x => x.trim.toInt ).toSet
        else Set[Int]()
    ).toArray
    val labels = readLabels(labelFile)
    val labelMap = buildLabelMapFromLabels(labels)
    new Graph(adjList, readLabels(labelFile), labelMap) 
  }

  def buildLabelMapFromLabels(labels: Array[Int]): 
                              Map[Int, Set[Int]] = {
    var labelMap = Map[Int, Set[Int]]()
    //TODO: possibly inefficient
    labels.foldLeft(0) ( (i, label) => { 
      labelMap = labelMap + (label -> (labelMap.getOrElse(label, Set[Int]()) + i))
      i + 1 
    })
    labelMap 
  }

  def readLabels(lFile: String) = 
    Source.fromFile(lFile).getLines.map(_.trim.toInt).toArray

}

object GraphWriterTest extends App {
  val g = GraphGenerator.generateRandomGraph(10, 5, 2)
  g.print
  g.writeToFile("g_test.txt")
}

object Neo4JWriterTest extends App {
  val g = GraphGenerator.generateRandomGraph(100000, 10, 10)
  //g.print
  g.writeToFileNeo4J("batch-import/tnode.csv", "batch-import/trel.csv")
}
