import scala.io._
import sys.process._

object YoutubeConverter extends App {
  val lines = Source.fromFile("4.txt").getLines.toArray
  var idMap = Map[String, Int]()
  var labelStr = Vector[String]()
  var labelSet = Set[String]()
  for (l <- lines) {
    if (!l.trim.equals("")) {
      val splitLine = l.split("\t").map(_.trim)
      if (splitLine.length >= 9) {
        val label = splitLine(3) 
        labelStr :+= label
        labelSet += label 
        idMap += (splitLine(0) -> idMap.size)
      }
    }
  }
  println(labelSet)
  val labelArr = labelSet.toArray
  val labels = labelStr.toArray.map(x => labelArr.indexOf(x))
  val labelMap = Graph.buildLabelMapFromLabels(labels) 
  val adjList = lines.foldLeft(Vector[Set[Int]]()) { (vec, x) =>
    val sx = x.split("\t").map(_.trim)
    if (sx.length > 9) {
      vec :+ sx.slice(10, sx.length).foldLeft(Set[Int]()){ (set, x) =>
        if(idMap.contains(x)) set + idMap(x) 
        else set 
      }
    } else if (sx.length == 9) {
      vec :+ Set[Int]() 
    } else { vec }
  }.toArray
  val g = new Graph(adjList, labels, labelMap)
  println("Youtube number of nodes: " + g.size)
  println("Number of edges:         " + g.nEdges)
  g.writeToFile("youg.txt")
  println("Done generating data graph")

  val querySizes = Array((2, 1), (4, 2), (6, 2), (8, 2), (10, 2), (20, 2), 
                         (40, 3), (60, 3), (80, 3), (100, 3))
  println("BFS queries")
  for ((s, deg) <- querySizes) {
    for (i <- 0 until 5) {
      val q = GraphGenerator.generateBFSQuery(s, deg, g)
      println("Query size:            " + s)
      println("Query average degree:  " + deg) 
      println("Query edges:           " + q.nEdges)
      q.writeToFile("youq.txt")
      println("Done generating query graph")
      var t0 = System.nanoTime()
      var sim = PatternMatcher.saltzDualSim(g, q)
      var t1 = System.nanoTime()

      var t3 = System.nanoTime()
      val sim3 = PatternMatcher.saltzIso3(g, q)
      var t4 = System.nanoTime()
      println("Saltz Dual Sim Took        " + (t1 - t0)/1000000.0 + " ms")
      println("Number of unique nodes d   " + sim.flatten.toSet.size)
      println("Saltz Iso 3 Took           " + (t4 - t3)/1000000.0 + " ms")
      println("Number of matches 3        " + sim3.size)
      println("Number of unique nodes 3   " + sim3.flatten.toSet.size)
      "./iso_exp.sh youg.txt youq.txt" #| "scala -cp node_counter/out/ NodeCounter" !
    }
  }

  println("RANDOM queries")
  for ((s, deg) <- querySizes) {
    for (i <- 0 until 5) {
      val q = GraphGenerator.generateBFSQuery(s, deg, g)
      println("Query size:            " + s)
      println("Query average degree:  " + deg) 
      println("Query edges:           " + q.nEdges)
      q.writeToFile("youq.txt")
      println("Done generating query graph")
      var t0 = System.nanoTime()
      val sim = PatternMatcher.saltzDualSim(g, q)
      var t1 = System.nanoTime()
      var t3 = System.nanoTime()
      val sim3 = PatternMatcher.saltzIso3(g, q)
      var t4 = System.nanoTime()
      println("Saltz Dual Sim Took        " + (t1 - t0)/1000000.0 + " ms")
      println("Number of unique nodes d   " + sim.flatten.toSet.size)
      println("Saltz Iso 3 Took           " + (t4 - t3)/1000000.0 + " ms")
      println("Number of matches 3        " + sim3.size)
      println("Number of unique nodes 3   " + sim3.flatten.toSet.size)
      "./iso_exp.sh youg.txt youq.txt" #| "scala -cp node_counter/out/ NodeCounter" !
    }
  }
}

object Main extends App {
  if (args.length != 4) { 
    println("Usage: <gLabelFile> <gEdgeFile> <qLabelFile> <qEdgeFile>")
    System.exit(-1)
  }
  val gLabelFile = args(0)
  val gEdgeFile = args(1)
  val qLabelFile = args(2)
  val qEdgeFile = args(3)
  val g = Graph(gLabelFile, gEdgeFile)
  g.writeToFile("us_g.txt")
  val q = Graph(qLabelFile, qEdgeFile)
  q.writeToFile("us_q.txt")
  PatternMatcher.saltzDualSim(g, q).foreach(println(_))

  val sim = PatternMatcher.saltzIso3(g, q)
  var i = 0
  sim.foreach{ x => 
    println("Result " + i) 
    x.foreach{ y => println(y) }
    println 
    i += 1 
  }

} 

object RGDualSimTest extends App {
  if (args.length != 3) {
    println("Usage: <g_file> <q_file>")
    System.exit(-1)
  }
  val g_file = args(0)
  val q_file = args(1)
  val sim_file = args(2)
  val g = GraphGenerator.generateRandomGraph(1000000, 200, 16)
  g.writeToFile(g_file)
  println("Done generating data graph")
  val q = GraphGenerator.generateBFSQuery(25, 3, g)
  q.writeToFile(q_file)
  println("Done generating query graph")
  var t0 = System.nanoTime()
  val sim = PatternMatcher.saltzDualSim(g, q)
  var t1 = System.nanoTime()
  PatternMatcher.writePhiToFile(sim, sim_file)
  println("Saltz Dual Sim took " + (t1 - t0)/1000000 + " ms")
  println("Found " + sim.flatten.toSet.size + " results.")
}

object PowerLawLabelIsoTest extends App {
  var t0 = System.nanoTime()
  val g = GraphGenerator.generateRandomGraphPLLabels(1000000, 200, 10)
  g.writeToFile("compg.txt")
  println("Done generating data graph")
  val q = GraphGenerator.generateBFSQuery(3, 2, g)
  q.writeToFile("compq.txt")
  println("Done generating query graph")
  var t1 = System.nanoTime()
  val sim = PatternMatcher.saltzIso3(g, q)
  var t2 = System.nanoTime()

  println("Graph creation time:           " + (t1 - t0)/1000000 + " ms")
  println("Saltz Iso Took                 " + (t2 - t1)/1000000 + " ms")
  println("Number of matches              " + sim.size)
  println("Number of unique nodes         " + sim.flatten.toSet.size)
}

object InducedTest extends App {
  val adjList = Array(Set[Int](1), Set[Int](2), Set[Int]())
  val labels = Array(0, 1, 2)
  val labelMap = Map((0,Set(0)), (1, Set(1)), (2, Set(2)))
  val q = new Graph(adjList, labels, labelMap)
  q.writeToFile("compq.txt")
  val gAdjList = adjList.map(x=>x) 
  gAdjList(2) = Set[Int](0)
  val g = new Graph(gAdjList, labels, labelMap)
  g.writeToFile("compg.txt")
  val sim = PatternMatcher.saltzIso3(g, q)
  sim.foreach{x => println(x.deep.mkString(", ")) }
}

object PowerGraphIsoTest extends App {
  var t0 = System.nanoTime()
  //TODO: how to find the average of a power law dist?
  //TODO: gaussian label dists and degree dists
  //TODO: set up experiments
  val g = GraphGenerator.generatePLGraphRandLabels(3700000, 418, 20, 2.1)
  g.writeToFile("compg.txt")
  println("Done generating data graph")
  val q = GraphGenerator.generateBFSQuery(10, 2, g)
  q.writeToFile("compq.txt")
  println("Done generating query graph")
  var t1 = System.nanoTime()
  val sim = PatternMatcher.saltzIso3(g, q)
  var t2 = System.nanoTime()

  println("Graph creation time:           " + (t1 - t0)/1000000 + " ms")
  println("Saltz Iso Took                 " + (t2 - t1)/1000000 + " ms")
  println("Number of matches              " + sim.size)
  println("Number of unique nodes         " + sim.flatten.toSet.size)
}

object MeVsUllmann extends App {
  var t0 = System.nanoTime()
  val g = GraphGenerator.generateRandomGraph(100, 1, 3)
  g.writeToFile("compg.txt")
  println("Done generating data graph")
  val q = GraphGenerator.generateBFSQuery(6, 2, g)
  q.writeToFile("compq.txt")
  println("Done generating query graph")
  var t1 = System.nanoTime()
  //val ullSim = PatternMatcher.ullmann(g, q)
  var t2 = System.nanoTime()
  val meSim = PatternMatcher.saltzIso3(g, q)
  var t3 = System.nanoTime()

  //var i = 0
/*  sim.foreach{ x => */
    //println("Result " + i) 
    //x.foreach{ y => println(y) }
    //println 
    //i += 1 
  /*}*/
  println("Graph creation time:       " + (t1 - t0)/1000000 + " ms")
  //println("Ullmann's Took             " + (t2 - t1)/1000000 + " ms")
  println("Saltz Iso 3 Took           " + (t3 - t2)/1000000 + " ms")
  //println("Number of matches ull      " + ullSim.size)
  println("Number of matches 3        " + meSim.size)
  //println("Number of unique nodes ull " + ullSim.flatten.toSet.size)
  println("Number of unique nodes 3   " + meSim.flatten.toSet.size)
}

object RandGraph extends App {
  var t0 = System.nanoTime()
  val g = GraphGenerator.generateRandomGraph(100000, 11, 10)
  g.writeToFile("compg.txt")
  println("Done generating data graph")
  val q = GraphGenerator.generateBFSQuery(10, 2, g)
  q.writeToFile("compq.txt")
  println("Done generating query graph")
  var t1 = System.nanoTime()
  var t3 = System.nanoTime()
  val sim3 = PatternMatcher.saltzIso3(g, q)
  var t4 = System.nanoTime()

  //var i = 0
/*  sim.foreach{ x => */
    //println("Result " + i) 
    //x.foreach{ y => println(y) }
    //println 
    //i += 1 
  /*}*/
  println("Graph creation time:       " + (t1 - t0)/1000000 + " ms")
  println("Saltz Iso 3 Took           " + (t4 - t3)/1000000 + " ms")
  println("Number of matches 3        " + sim3.size)
  println("Number of unique nodes 3   " + sim3.flatten.toSet.size)
 
}
object SaltzIsoTest extends App {
  if (args.length != 7) {
    println("""Usage: <g_file> <q_file> <g_size> 
             <n_labels> <av_g_degree> <q_size> 
             <av_q_degree>""")
    System.exit(-1)
  }

  val g_file = args(0)
  val q_file = args(1)
  val g_size = args(2).toInt
  val n_labels = args(3).toInt
  val av_g_degree = args(4).toInt
  val q_size = args(5).toInt
  val av_q_degree = args(6).toInt
  var t0 = System.nanoTime()
  val g = GraphGenerator.generateRandomGraph(g_size, n_labels, av_g_degree)
  g.writeToFile(g_file)
  //println("Done generating data graph")
  val q = GraphGenerator.generateBFSQuery(q_size, av_q_degree, g)
  q.writeToFile(q_file)
  //println("Done generating query graph")
  var t1 = System.nanoTime()
  val sim = PatternMatcherOld.saltzIso(g, q)
  var t2 = System.nanoTime()
  //var i = 0
/*  sim.foreach{ x => */
    //println("Result " + i) 
    //x.foreach{ y => println(y) }
    //println 
    //i += 1 
  /*}*/
  println("Scala Graph creation time:       " + (t1 - t0)/1000000 + " ms")
  println("Scala Saltz Iso Took             " + (t2 - t1)/1000000 + " ms")
  println("Scala Total time:                " + (t2 - t0)/1000000 + " ms")
  println("Scala number of matches          " + sim.size)
  println("Scala number of unique nodes     " + sim.flatten.toSet.size)
}
