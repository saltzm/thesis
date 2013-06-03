
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
  if (args.length != 3) println("Usage: <g_file> <q_file>")
  val g_file = args(0)
  val q_file = args(1)
  val sim_file = args(2)
  val g = GraphGenerator.generateRandomGraph(1000000, 20, 16)
  g.writeToFile(g_file)
  println("Done generating data graph")
  val q = GraphGenerator.generateBFSQuery(4, 2, g)
  q.writeToFile(q_file)
  println("Done generating query graph")
  var t0 = System.nanoTime()
  val sim = PatternMatcher.saltzDualSim(g, q)
  var t1 = System.nanoTime()
  PatternMatcher.writePhiToFile(sim, sim_file)
  println("Saltz Dual Sim took " + (t1 - t0)/1000000 + " ms")
  println("Found " + sim.flatten.toSet.size + " results.")
}
