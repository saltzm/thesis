import scala.util.Random
import scala.collection.mutable.Queue

object GraphGenerator {
  val rand = new Random
  def generateRandomGraph(size: Int, nLabels: Int, avDegree: Int): Graph = {
    val adjList = Array.ofDim[Set[Int]](size).map( node => {
      val degree = rand.nextInt(avDegree * 2 + 1)
      (0 until degree).map( _ => rand.nextInt(size) ).toSet
    })
    val labels = randDistLabels(size, nLabels)
    var labelMap = Graph.buildLabelMapFromLabels(labels) 
    new Graph(adjList, labels, labelMap)
  }

  def generateRandomGraphPLLabels(size: Int, nLabels: Int, avDegree: Int): Graph = {
    val adjList = Array.ofDim[Set[Int]](size).map( node => {
      val degree = rand.nextInt(avDegree * 2 + 1)
      (0 until degree).map( _ => rand.nextInt(size) ).toSet
    })
    // 2.1 is used in WWW graph pg 72 of m&m graph data
    val labels = powDistLabels(size, nLabels, 2.1)
    var labelMap = Graph.buildLabelMapFromLabels(labels) 
    new Graph(adjList, labels, labelMap)
  }

  def generatePLGraphRandLabels(size: Int, nLabels: Int, maxDegree: Int, 
                                distPow: Double): Graph = {
    var nEdges = 0
    val adjList = Array.ofDim[Set[Int]](size).map( node => {
      val degree = powInt(0, maxDegree, distPow)
      nEdges += degree
      (0 until degree).map( _ => rand.nextInt(size)).toSet
    })
    val labels = randDistLabels(size, nLabels)
    var labelMap = Graph.buildLabelMapFromLabels(labels) 
    println("Nodes:  " + size)
    println("Edges:  " + nEdges)
    new Graph(adjList, labels, labelMap)
  }

  def generatePLGraphPowLabels(size: Int, nLabels: Int, maxDegree: Int, 
                                distPow: Double): Graph = {
    val adjList = Array.ofDim[Set[Int]](size).map( node => {
      val degree = powInt(0, maxDegree, distPow)
      (0 until degree).map( _ => rand.nextInt(size) ).toSet
    })
    val labels = powDistLabels(size, nLabels, distPow)
    var labelMap = Graph.buildLabelMapFromLabels(labels) 
    new Graph(adjList, labels, labelMap)
  }

  def randDistLabels(size: Int, nLabels: Int) = 
    Array.ofDim[Int](size).map( x => rand.nextInt(nLabels) )
  def powDistLabels(size: Int, nLabels: Int, pow: Double) = 
    Array.ofDim[Int](size).map( x => powInt(0, nLabels, pow) )
  def gaussianDistLabels(size: Int, nLabels: Int) = 
    Array.ofDim[Int](size).map( x => gaussInt(nLabels/2.0) ) 

  def powInt(min: Int, max: Int, distPow: Double): Int = {
    max - 1 - math.pow(((math.pow(max, distPow + 1) - 
               math.pow(min, distPow + 1))*rand.nextDouble + 
               math.pow(min, distPow + 1)), 
               (1.0/(distPow + 1))).toInt 
  }
  // d: average value of the distribution
  def gaussInt(d: Double) = 
    math.min(math.max((rand.nextGaussian()*d+d).toInt, 0), d*2).toInt

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
            val newChild = nncArr.apply(rand.nextInt(newNodeChildren.size)) 
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
  var g = GraphGenerator.generateRandomGraph(100, 100, 2)
  g.print
}

object PowerLawLabelTest extends App {
  var g = GraphGenerator.generateRandomGraphPLLabels(200, 50, 2)
  //g.print
  g.labelMap.toSeq.sortBy(_._1).foreach{ println(_) }
}

object PLGraphRandLabelTest extends App {
  var g = GraphGenerator.generatePLGraphRandLabels(50, 10, 10, 2.1)
  g.adjList.sortBy(_.size).foreach{ println(_) }
}

object PLGraphPowLabelTest extends App {
  var g = GraphGenerator.generatePLGraphPowLabels(50, 10, 10, 2.1)
  g.adjList.sortBy(_.size).foreach{ println(_) }
  g.labelMap.toSeq.sortBy(_._1).foreach{ println(_) }
}

object QueryGenerationTest extends App {
  var g = GraphGenerator.generateRandomGraph(10, 10 , 3)
  g.print
  println
  println
  var q = GraphGenerator.generateBFSQuery(5, 2, g)
  q.print
}
