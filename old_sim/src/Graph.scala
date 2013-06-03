import collection.mutable.{Map, Set}
import scala.io._

class Graph(fp: String = "", nVerts: Int = 0) {
  var n = nVerts // number of veritces in the graph
  var adjSet = Array[Set[Int]]()
  var labels = Array[Int]()
  var labelMap = Map[Int, Set[Int]]()
  var parList = Array[Set[Int]]()

  if ( fp != "" ){
    val source = Source.fromFile(fp).getLines().toArray
    n = source.length
    adjSet = Array.ofDim[Set[Int]](n)
    labels = Array.ofDim[Int](n)
    labelMap = Map[Int, Set[Int]]()
    parList = Array.ofDim[Set[Int]](n)
    for (i <- 0 until n) adjSet(i) = Set[Int]()
    for (i <- 0 until n) parList(i) = Set[Int]()

    for (x <- source) {
      val parts = x.split(' ')
      val id = parts(0).toInt
      labels(id) = parts(1).toInt
      for (i <- 2 until parts.size) {
        val cid = parts(i).toInt
        adjSet(id) += cid
        parList(cid) += id
      }
      labelMap.getOrElseUpdate(labels(id), Set(id)) += id
    }
  } else{
    adjSet = Array.ofDim[Set[Int]](n)
    labels = Array.ofDim[Int](n)
    labelMap = Map[Int, Set[Int]]()
    parList = Array.ofDim[Set[Int]](n)
    for (i <- 0 until n) adjSet(i) = Set[Int]()
    for (i <- 0 until n) parList(i) = Set[Int]()
  }

  override def clone(): Graph= {
    val g = new Graph("", n)
    g.adjSet = this.adjSet.clone()
    g.labels = this.labels.clone()
    g.labelMap = this.labelMap.clone()
    g.parList = this.parList.clone()
    g
  }


  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Returns the set of children of a vertex
    * @param id the identifier of the input vertex
    */
  def post(id: Int): Set[Int] = {
    adjSet(id).clone()
  }

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Returns the set of parents of a vertex
    * @param id the identifier of the input vertex
    */
  def pre(id: Int): Set[Int] = {
    parList(id).clone()
  }

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Returns diameter of graph using floyd-warshall
    */
  def getDiameter: Int = {

    var path = Array.ofDim[Int](n, n)

    for (u <- 0 until n; v <- 0 until n if u != v) {
      if (post(u).contains(v)) {
        path(v)(u) = 1
        path(u)(v) = 1
      }
    }        

    var diameter = 0;

    for (k <- 0 until n; i <- 0 until n; j <- 0 until n if i != j) {
      if (path(i)(k) * path(k)(j) != 0)
        if ((path(i)(k) + path(k)(j) < path(i)(j)) || path(i)(j) == 0 ){
          path(i)(j) = path(i)(k) + path(k)(j)
        }

      diameter = math.max(path(i)(j), diameter)
    }

    diameter    
  }

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Dump the whole graph
    */
  def display() {
    println("\n\n********Dump of graph " + fp + "********")

    println("\n-- Labels -- ")
    println(labels.deep)

    println("\n-- Edges  --")
    println(adjSet.deep)
  }

}
