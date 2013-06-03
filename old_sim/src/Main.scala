import collection.mutable

object Main extends App {

  if (args.length != 4) {
    println("args must be in the format:  <data_graph> <query_graph> <print match/ball (0/1)> <strong/strict (strong/strict)>")
    System.exit(0)
  }

  val gfile = args(0)
  val qfile = args(1)
  val printSetting = args(2)
  val strongSetting = args(3)

  val g = new Graph(gfile)
  val q = new Graph(qfile)
  val gs = new GraphSimulator(printSetting, strongSetting)
  var sim = new mutable.HashMap[Int, mutable.Set[Int]]()
  var t0 = System.nanoTime()
  sim = gs.strongSim(q, g, qfile, gfile)
  var t1 = System.nanoTime()
  println("        Total strong sim time:  " + ((t1 - t0) / 1000000.0) + " ms")

}
