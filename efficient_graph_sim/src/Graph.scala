import scala.io._
import java.io._

/** A class representing a Graph 
  */
case class Graph(adjList: Array[Set[Int]], 
                 labels: Array[Int], 
                 labelMap: Map[Int, Set[Int]]) {
  def size = adjList.size
  def children(id: Int) = adjList(id)
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
