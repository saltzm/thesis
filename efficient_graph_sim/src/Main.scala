
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
object IsoPrintTest extends App {
  var t0 = System.nanoTime()
  val g = GraphGenerator.generateRandomGraph(1000, 10, 6)
  g.writeToFile("compg.txt")
  println("Done generating data graph")
  val q = GraphGenerator.generateBFSQuery(10, 2, g)
  q.writeToFile("compq.txt")
  println("Done generating query graph")
  var t1 = System.nanoTime()
  val n = PatternMatcher.saltzIsoPrint(g, q, "testres.txt")
  var t2 = System.nanoTime()

  println("Graph creation time:           " + (t1 - t0)/1000000 + " ms")
  println("Saltz Iso Print Took           " + (t2 - t1)/1000000 + " ms")
  println("Number of matches              " + n)
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

object IsoCompTest extends App {
  var t0 = System.nanoTime()
  val g = GraphGenerator.generateRandomGraph(100, 5, 3)
  g.writeToFile("compg.txt")
  println("Done generating data graph")
  val q = GraphGenerator.generateBFSQuery(3, 2, g)
  q.writeToFile("compq.txt")
  println("Done generating query graph")
  var t1 = System.nanoTime()
  //val sim = PatternMatcher.saltzIso(g, q)
  var t2 = System.nanoTime()
  //val sim2 = PatternMatcher.saltzIso2(g, q)
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
  //println("Saltz Iso Took             " + (t2 - t1)/1000000 + " ms")
  //println("Saltz Iso 2 Took           " + (t3 - t2)/1000000 + " ms")
  println("Saltz Iso 3 Took           " + (t4 - t3)/1000000 + " ms")
  //println("Number of matches          " + sim.size)
  //println("Number of matches 2        " + sim2.size)
  println("Number of matches 3        " + sim3.size)
  //println("Number of unique nodes     " + sim.flatten.toSet.size)
  //println("Number of unique nodes 2   " + sim2.flatten.toSet.size)
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
  val sim = PatternMatcher.saltzIso(g, q)
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
