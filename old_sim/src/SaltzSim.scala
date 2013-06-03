import java.io._

object DualSimTest extends App {
  if (args.length != 3) {
    println("args must be in the format: <data_graph> <query_graph> <sim_file>")
    System.exit(0)
  }

  val gfile = args(0)
  val qfile = args(1)
  val simfile = args(2)
  val g = new Graph(gfile)
  val q = new Graph(qfile)
  var t1 = System.nanoTime()
  val dSim = DualSimulator.dualSim(q, g)._2
  val t2 = System.nanoTime()
  DualSimulator.writeSimToFile(dSim, simfile)
  println("Dual Results Size: " + dSim.flatten.toSet.size)
  println("Total dual sim time:  " + ((t2 - t1) / 1000000.0) + " ms")
}

object DualSimulator { 
  def writeSimToFile(sim: Array[Set[Int]], fileName: String) {
    val out = new PrintWriter(fileName)
    sim.foreach { p =>
      out.println(p)
    }
    out.close
  }

  def dualSim(q: Graph, g: Graph, initSim: Array[Set[Int]]): 
             (Boolean, Array[Set[Int]]) = {
    // sets every item in sim equal to the nodes in g with the same
    // label as node i in the query
    // TODO: lots of cloning = meh
    var sim = initSim.clone 
   
    var flag = true
    while (flag) {
      flag = false
      for (u <- 0 until q.n; w <- sim(u); v <- q.post(u) if (g.post(w) & sim(v)).isEmpty) {
        sim(u) -= w
        if (sim(u).isEmpty) return (false, Array[Set[Int]]())
        flag = true
      }
      for (u <- 0 until q.n; w <- sim(u); v <- q.pre(u) if (g.pre(w) & sim(v)).isEmpty) {
        sim(u) -= w
        if (sim(u).isEmpty) return (false, Array[Set[Int]]())
        flag = true
      }
    }
    (true, sim)
  }

  def dualSim(q: Graph, g: Graph): (Boolean, Array[Set[Int]]) = {
    // sets every item in sim equal to the nodes in g with the same
    // label as node i in the query
    val sim = Array.ofDim[Set[Int]](q.n)
    for (i <- sim.indices) sim(i) = g.labelMap(q.labels(i)).toSet
    dualSim(q, g, sim)
  }
}
