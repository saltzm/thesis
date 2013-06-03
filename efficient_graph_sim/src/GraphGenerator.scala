import scala.util.Random
import scala.collection.mutable.Queue

object GraphGenerator {
  val rand = new Random
  //TODO: implement power law generator
  //TODO: implement power law distribution for labels
  //TODO: implement gaussian generator
  def generateRandomGraph(size: Int, nLabels: Int, avDegree: Int): Graph = {
    val adjList = Array.ofDim[Set[Int]](size).map( node => {
      val degree = rand.nextInt(avDegree * 2 + 1)
      (0 until degree).map( _ => rand.nextInt(size) ).toSet
    })
    val labels = Array.ofDim[Int](size).map( x => rand.nextInt(nLabels) )
    var labelMap = Graph.buildLabelMapFromLabels(labels) 
    new Graph(adjList, labels, labelMap)
  }
  
  def generateRandomConnectedGraph(size: Int, nLabels: Int, avDegree: Int): Graph = {
    val adjList = Array.ofDim[Set[Int]](size).map( node => {
      // min degree 1 guarantees connectedness
      val degree = 1 + rand.nextInt(avDegree * 2 + 1)
      (0 until degree).map( _ => rand.nextInt(size) ).toSet
    })
    val labels = Array.ofDim[Int](size).map( x => rand.nextInt(nLabels) )
    var labelMap = Graph.buildLabelMapFromLabels(labels) 
    new Graph(adjList, labels, labelMap)
  }

 def generateBFSQuery(size: Int, avDegree: Int, g: Graph): Graph = {
   /* //randomly pick a start node in adjList*/
    var adjMap = Map[Int, Set[Int]]()
    var nodes = Set[Int]()
    val maxRestarts = 5000
    var nRestarts = 0
    while (nodes.size < size && nRestarts < maxRestarts) {
      if (nRestarts % 100 == 0) println("restarting " + nRestarts)
      adjMap = Map[Int, Set[Int]]()
      nodes = Set[Int]()
      val q = Queue[Int]()
      val start = rand.nextInt(g.size)
      q.enqueue(start)
      nodes += start
      var infinite = false 
      while (!q.isEmpty && nodes.size < size && !infinite ) {
        infinite = true
        var adjs = Set[Int]()
        val newNode = q.dequeue
        val newNodeChildren = g.adjList(newNode)
        if (!newNodeChildren.isEmpty) {
          val nncArr = newNodeChildren.toArray
          for(i <- 0 until rand.nextInt(avDegree * 2 + 1) if nodes.size < size) {
            val newChild = nncArr.apply(
              rand.nextInt(newNodeChildren.size)
            ) 
            if (!(nodes contains newChild)) infinite = false

            nodes += newChild
            adjs += newChild 
             
            q.enqueue(newChild)
          }
          adjMap += (newNode -> (adjMap.getOrElse(newNode, Set[Int]()) ++ adjs))
        }
      }
      if(infinite || nodes.size < size) nRestarts += 1
    }
    if (nRestarts == maxRestarts) { 
      println("could not find good query")
      System.exit(-1)
    }

    val iter = nodes.iterator
    // gives the nodes new ids
    var newLabelMap = Map[Int, Int]() 
    var c = 0 
    for (x <- nodes) { 
      newLabelMap += (x -> c)
      c += 1
    }
    val newToOldLabels = Array.ofDim[Int](size)
    newLabelMap.foreach{ case(oldL, newL) => 
      newToOldLabels(newL) = oldL 
    }
    val adjList = Array.ofDim[Set[Int]](size).map(x => Set[Int]())
    for ((node, children) <- adjMap) {
      adjList(newLabelMap(node)) = children.map(x => newLabelMap(x)) 
    }
    val labels = newToOldLabels.map(x => g.labels(x)).toArray
    val labelMap = Graph.buildLabelMapFromLabels(labels)     
    Graph(adjList, labels, labelMap)
  }
}

object RandomGraphTest extends App {
  var g = GraphGenerator.generateRandomGraph(10, 5, 2)
  g.print
}

object QueryGenerationTest extends App {
  var g = GraphGenerator.generateRandomGraph(10, 10 , 3)
  g.print
  println
  println
  var q = GraphGenerator.generateBFSQuery(5, 2, g)
  q.print
}
