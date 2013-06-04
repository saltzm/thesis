
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
  val q = Graph(qLabelFile, qEdgeFile)
  PatternMatcher.saltzDualSim(g, q).foreach(println(_))
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

object SaltzIsoTest extends App {
  val g_file = args(0)
  val q_file = args(1)
  var t0 = System.nanoTime()
  val g = GraphGenerator.generateRandomGraph(10000, 20, 16)
  g.writeToFile(g_file)
  println("Done generating data graph")
  val q = GraphGenerator.generateBFSQuery(4, 2, g)
  q.writeToFile(q_file)
  println("Done generating query graph")
  var t1 = System.nanoTime()
  val sim = PatternMatcher.saltzIso(g, q)
  var t2 = System.nanoTime()
  var i = 0
 sim.foreach{ x => 
    println("Result " + i) 
    x.foreach{ y => println(y) }
    println 
    i += 1 
  }
  println("Graph creation time:       " + (t1 - t0)/1000000 + " ms")
  println("Saltz Iso Took             " + (t2 - t1)/1000000 + " ms")
  println("Total time:                " + (t2 - t0)/1000000 + " ms")
  println("Found " + sim.size + " isomorphic matches.")
  println("Found " + sim.flatten.toSet.size + " unique nodes.")
 
}
