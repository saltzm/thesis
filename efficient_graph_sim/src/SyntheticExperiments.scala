import sys.process._
import scala.language.postfixOps

object VaryLabelsSmall extends App {
  val labelRange = 1 to 10
  val repetitions = 0 until 1
  val querySize = 4
  val queryAvDegree = 2
  val graphSize = 10000
  val graphAvDegree = 6
  val power = 2.1
  for (l <- labelRange) { 
    println("N LABELS: " + l)
    // TODO:  Should I do multiple queries for each data graph?
    for (r <- repetitions) {
      // RAND LABEL TESTS
      println("Generating rand graph...")
      var g = GraphGenerator.generateRandomGraph(graphSize, l, graphAvDegree)
      g.writeToFile("lg.txt")
      println("Done generating rand graph...")

      println("Generating query graph...")
      var q = GraphGenerator.generateBFSQuery(querySize, queryAvDegree, g)
      q.writeToFile("lq.txt")
      println("Done generating query...")

      Utils.dualExp(g, q, "rand")
      Utils.saltzExp(g, q, "rand")
      Utils.ullExp(g, q, "rand")
      Utils.vf2Exp("lg.txt", "lq.txt")
      // END RAND LABEL TESTS

      // BEGIN POWER LABEL TESTS
      println("Creating graph pow...")
      val powLabels = GraphGenerator.powDistLabels(g.size, l, power)
      g = new Graph(g.adjList, powLabels, 
                    Graph.buildLabelMapFromLabels(powLabels))
      g.writeToFile("lg.txt")
      println("Done creating graph pow...")

      println("Creating query graph...")
      q = GraphGenerator.generateBFSQuery(querySize, queryAvDegree, g)
      q.writeToFile("lq.txt")
      println("Done creating query graph...")

      Utils.dualExp(g, q, "pow")
      Utils.saltzExp(g, q, "pow")
      Utils.ullExp(g, q, "pow")
      Utils.vf2Exp("lg.txt", "lq.txt")
      //END POWER LABEL TESTS

      //BEGIN GAUSS LABEL TESTS
      val gaussLabels = GraphGenerator.gaussianDistLabels(g.size, l)
      g = new Graph(g.adjList, gaussLabels, 
                    Graph.buildLabelMapFromLabels(gaussLabels))
      g.writeToFile("lg.txt")
      q = GraphGenerator.generateBFSQuery(querySize, queryAvDegree, g)
      q.writeToFile("lq.txt")

      Utils.dualExp(g, q, "gauss")
      Utils.saltzExp(g, q, "gauss")
      Utils.ullExp(g, q, "gauss")
      Utils.vf2Exp("lg.txt", "lq.txt")
      // END GAUSS LABEL TESTS
    }
  }
}

object VaryLabelsLarge extends App {
  val labelRange = Array(20, 30, 40, 50, 100, 200, 300, 400, 500) 
  val repetitions = 0 until 1
  val querySize = 10 
  val queryAvDegree = 2
  val graphSize = 1000000
  val graphAvDegree = 16 
  val power = 2.1
  for (l <- labelRange) { 
    println("N LABELS: " + l)
    // TODO:  Should I do multiple queries for each data graph?
    for (r <- repetitions) {
      // RAND LABEL TESTS
      println("Generating rand graph...")
      var g = GraphGenerator.generateRandomGraph(graphSize, l, graphAvDegree)
      g.writeToFile("lg.txt")
      println("Done generating rand graph...")

      println("Generating query graph...")
      var q = GraphGenerator.generateBFSQuery(querySize, queryAvDegree, g)
      q.writeToFile("lq.txt")
      println("Done generating query...")

      Utils.dualExp(g, q, "rand")
      Utils.saltzExp(g, q, "rand")
      //Utils.ullExp(g, q, "rand")
      Utils.vf2Exp("lg.txt", "lq.txt")
      // END RAND LABEL TESTS

      // BEGIN POWER LABEL TESTS
      println("Creating graph pow...")
      val powLabels = GraphGenerator.powDistLabels(g.size, l, power)
      g = new Graph(g.adjList, powLabels, 
                    Graph.buildLabelMapFromLabels(powLabels))
      g.writeToFile("lg.txt")
      println("Done creating graph pow...")

      println("Creating query graph...")
      q = GraphGenerator.generateBFSQuery(querySize, queryAvDegree, g)
      q.writeToFile("lq.txt")
      println("Done creating query graph...")

      Utils.dualExp(g, q, "pow")
      Utils.saltzExp(g, q, "pow")
      //Utils.ullExp(g, q, "pow")
      Utils.vf2Exp("lg.txt", "lq.txt")
      //END POWER LABEL TESTS

      //BEGIN GAUSS LABEL TESTS
      val gaussLabels = GraphGenerator.gaussianDistLabels(g.size, l)
      g = new Graph(g.adjList, gaussLabels, 
                    Graph.buildLabelMapFromLabels(gaussLabels))
      g.writeToFile("lg.txt")
      q = GraphGenerator.generateBFSQuery(querySize, queryAvDegree, g)
      q.writeToFile("lq.txt")

      Utils.dualExp(g, q, "gauss")
      Utils.saltzExp(g, q, "gauss")
      //Utils.ullExp(g, q, "gauss")
      Utils.vf2Exp("lg.txt", "lq.txt")
      // END GAUSS LABEL TESTS
    }
  }

}

object VaryVqSmall extends App {
  val l = 200
  val repetitions = 0 until 10
  val outerReps = 0 until 5
  val querySizes = Array(2, 4, 6, 8, 10, 12, 14, 16, 18, 20) 
  val queryAvDegree = 2
  val graphSize = 1000000
  val graphAvDegree = 16 

  for (o <- outerReps) {
    println("Generating rand graph...")
    var g = GraphGenerator.generateRandomGraph(graphSize, l, graphAvDegree)
    g.writeToFile("vqgsmall.txt")
    println("Done generating rand graph...")

    for (vq <- querySizes) {
      println("QUERY SIZE: " + vq)
      for (r <- repetitions) {
        println("Generating query graph...")
        var q = GraphGenerator.generateBFSQuery(vq, queryAvDegree, g)
        q.writeToFile("vqqsmall.txt")
        println("Done generating query...")

        Utils.dualExp(g, q, "rand")
        Utils.saltzExp(g, q, "rand")
        Utils.vf2Exp("vqgsmall.txt", "vqqsmall.txt")
      }
    }
  }
}

object VaryVqLarge extends App {
  val l = 200
  val repetitions = 0 until 10
  val outerReps = 0 until 5
  val querySizes = Array(20, 30, 40, 50, 100, 200, 300, 400, 500) 
  val queryAvDegree = 2
  val graphSize = 1000000
  val graphAvDegree = 16 

  for (o <- outerReps) {
    println("Generating rand graph...")
    var g = GraphGenerator.generateRandomGraph(graphSize, l, graphAvDegree)
    g.writeToFile("vqglarge.txt")
    println("Done generating rand graph...")

    for (vq <- querySizes) {
      println("QUERY SIZE: " + vq)
      for (r <- repetitions) {
        println("Generating query graph...")
        var q = GraphGenerator.generateBFSQuery(vq, queryAvDegree, g)
        q.writeToFile("vqqlarge.txt")
        println("Done generating query...")

        Utils.dualExp(g, q, "rand")
        Utils.saltzExp(g, q, "rand")
        Utils.vf2Exp("vqglarge.txt", "vqqlarge.txt")
      }
    }
  }

}

object VaryVgRandDegree extends App {
  val l = 200
  val repetitions = 0 until 10
  val outerReps = 0 until 5
  val querySize = 10
  val queryAvDegree = 2
  val graphSizes = Array(1000, 10000, 100000, 1000000, 10000000)
  val graphAvDegrees = graphSizes.map(math.pow(_, 0.2).toInt) 
  val graphExps = graphSizes zip graphAvDegrees

  for ((vg, deg) <- graphExps) {
    println(s"GRAPH SIZE: $vg   DEGREE: $deg")
    for (o <- outerReps) {
      println(s"Outer rep $o\n")
      println("Generating rand graph...")
      var g = GraphGenerator.generateRandomGraph(vg, l, deg)
      g.writeToFile("vgrand.txt")
      println("Done generating rand graph...")

      for (r <- repetitions) {
        println("Generating query graph...")
        var q = GraphGenerator.generateBFSQuery(querySize, queryAvDegree, g)
        q.writeToFile("vqrand.txt")
        println("Done generating query...")

        Utils.dualExp(g, q, "rand")
        Utils.saltzExp(g, q, "rand")
        Utils.vf2Exp("vgrand.txt", "vqrand.txt")
      }
    }
  }

}

object VaryVgPowDegree extends App {
  val l = 200
  val repetitions = 0 until 10
  val outerReps = 0 until 5
  val querySize = 10
  val queryAvDegree = 2
  val graphSizes = Array(1000, 10000, 100000, 1000000, 10000000)
  val graphMaxDegrees = graphSizes.map(x=>(math.pow(x, 0.2)*4.41).toInt) 
  val graphExps = graphSizes zip graphMaxDegrees
  val pow = 2.1

  for ((vg, deg) <- graphExps) {
    println(s"GRAPH SIZE: $vg   DEGREE: $deg")
    for (o <- outerReps) {
      println(s"Outer rep $o\n")
      println("Generating pow graph...")
      var g = GraphGenerator.generatePLGraphRandLabels(vg, l, deg, pow)
      g.writeToFile("vgpow.txt")
      println("Done generating pow graph...")

      for (r <- repetitions) {
        println("Generating query graph...")
        var q = GraphGenerator.generateBFSQuery(querySize, queryAvDegree, g)
        q.writeToFile("vqpow.txt")
        println("Done generating query...")

        Utils.dualExp(g, q, "pow")
        Utils.saltzExp(g, q, "pow")
        Utils.vf2Exp("vgpow.txt", "vqpow.txt")
      }
    }
  }
}

object Utils {
  def dualExp(g: Graph, q: Graph, name: String) {
    println("Beginning dual sim...")
    val (sim, time) = Utils.time {
      PatternMatcher.saltzDualSim(g, q)
    }
    println("Dual " + name + " took:         " + time + " ms")
    println("Dual " + name + " min phi size: " + sim.map(_.size).min)
    println("Dual " + name + " max phi size: " + sim.map(_.size).max)
    println("Dual " + name + " unique nodes: " + sim.flatten.toSet.size)
  }

  def saltzExp(g: Graph, q: Graph, name: String) {
    println("Beginning Saltz Iso...") 
    val (sim, time) = Utils.time {
      PatternMatcher.saltzIso3(g, q)
    }
    println("Saltz Iso " + name + " took:         " + time + " ms")
    println("Saltz Iso " + name + " matches:      " + sim.size)
    println("Saltz Iso " + name + " unique nodes: " + sim.flatten.toSet.size)
  }

  def ullExp(g: Graph, q: Graph, name: String) {
    println("Beginning Ullmann's...")
    val (sim, time) = Utils.time {
      PatternMatcher.ullmann(g, q)
    }
    println("Ullmann " + name + " took:         " + time + " ms")
    println("Ullmann " + name + " matches:      " + sim.size)
    println("Ullmann " + name + " unique nodes: " + sim.flatten.toSet.size)
  }

  def vf2Exp(gName: String, qName: String) {
    println("Beginning VF2...")
    s"./iso_exp.sh $gName $qName" #| "scala -cp node_counter/out/ NodeCounter" !
  }

  def time[R](block: => R): (R, Double) = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    (result, (t1 - t0)/1000000.0)
  }
}

