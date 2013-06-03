import collection.mutable.{HashMap, Map, Set, Stack}
import collection.mutable
import util.control.Breaks._
import actors.Actor._

class GraphSimulator(printSetting: String, strongSim: String) {

  def strongSim(q: Graph, g: Graph, queryName: String, dataName: String): HashMap[Int, Set[Int]] = {

    var dataSize = g.adjSet.length
    var querySize = q.adjSet.length

    var (sim, dualTime) = time {dualSim(q, g)};
    println("DUAL RESULT SIZE: "+sim.values.flatten.toSet.size)
    //printDualSimMatch(sim)
    if (sim.size == 0) { println("No dual match."); System.exit(0) }

    var newGraph = g
    var graphPruningTime = 0.0

    if( strongSim.equals("strict")){
      println("doing strict")
      var pair = time {filterGraph(g, q,  sim)} //if doing strong sim more than once, must clone g
      newGraph = pair._1
      graphPruningTime = pair._2
    } else println("doing strong")

    var prunedSize = sim.values.flatten.toSet.size
    val (qDiameter, diameterTime) = time { q.getDiameter }

    //for printing purposes
    val balls = HashMap[Int, Ball]()
    var matchCenters = Set[Int]()

    var ballTime = 0.0
    var filterTime = 0.0
    var ballSum = 0
    var nodesInDualSimsets = sim.values.flatten.toSet

    for (center <- nodesInDualSimsets) {
      val (ball, bTime) = time {new Ball(newGraph, center, qDiameter)}
      ballSum += ball.nodesInBall.size
      val (mat, fTime) = time {dualFilter(q, sim.clone(), ball)}
      ballTime += bTime
      filterTime += fTime

      balls.put(center, ball)
      if (mat.size != 0) {
        matchCenters += center
        //printArash(center, mat)
      } else {
        println("No match for ball centered at " + center + ".")
        println("-----------")
      }
    }
    //printUsman(g, balls, matchCenters)

    println("SEQUENTIAL: \nData graph: \t" + dataName + "\t\tNumber of nodes:  " + dataSize +
      "\nQuery graph: \t" + queryName + "\t\tNumber of nodes:  " + querySize +
      "\nNumber of " + strongSim + " matches: \t" + matchCenters.size +
      "\n\n    dualSim over entire graph:  " + dualTime + " ms" +
      "\n                Graph pruning:  " + graphPruningTime + " ms" +
      "\n     Graph size after pruning:  " + prunedSize + " nodes" +
      "\n       Finding query diameter:  " + diameterTime + " ms" +
      "\n               Query diameter:  " + qDiameter +
      "\n                    Ball time:  " + ballTime +
      "\n            Average ball size:  " + (ballSum/prunedSize.toDouble) +
      "\n                  Filter time:  " + filterTime + 
      "\n Creating and filtering balls:  " + (ballTime + filterTime) + " ms")
    sim
  }
  
  def dualSim(q: Graph, g: Graph, initSim: HashMap[Int, Set[Int]]): HashMap[Int, Set[Int]] = {
    var sim = initSim

    var flag = true
    while (flag) {
      flag = false
      for (u <- 0 until q.n; w <- sim(u); v <- q.post(u) if (g.post(w) & sim(v)).isEmpty) {
        sim(u) -= w
        if (sim(u).isEmpty) {
          println("u: " + u)
          println("v: " + v)
          println("w: " + w)
          println("g.post(w): " + g.post(w))
          println("sim(v): " + sim(v))
          return HashMap[Int, Set[Int]]()
        }
        flag = true
      }
      for (u <- 0 until q.n; w <- sim(u); v <- q.pre(u) if (g.pre(w) & sim(v)).isEmpty) {
        sim(u) -= w
        if (sim(u).isEmpty) {
          println("g.labelMap: "+g.labelMap(q.labels(u)))
          println("u: " + u)
          println("v: " + v)
          println("w: " + w)
          println("g.pre(w): " + g.pre(w))
          println("sim(v): " + sim(v))
          return HashMap[Int, Set[Int]]()
        }
        flag = true
      }
    }

    sim
  }

  def dualSim(q: Graph, g: Graph): HashMap[Int, Set[Int]] = {

    val sim = HashMap[Int, Set[Int]]()

    for (i <- 0 until q.n) sim += (i -> g.labelMap(q.labels(i)).clone())
    println("INIT SIM SIZE: " + sim.values.flatten.toSet.size)

    var flag = true
    while (flag) {
      flag = false
      for (u <- 0 until q.n; w <- sim(u); v <- q.post(u) if (g.post(w) & sim(v)).isEmpty) {
        sim(u) -= w
        if (sim(u).isEmpty) {
          println("u: " + u)
          println("v: " + v)
          println("w: " + w)
          println("g.post(w): " + g.post(w))
          println("sim(v): " + sim(v))
          return HashMap[Int, Set[Int]]()
        }
        flag = true
      }
      for (u <- 0 until q.n; w <- sim(u); v <- q.pre(u) if (g.pre(w) & sim(v)).isEmpty) {
        sim(u) -= w
        if (sim(u).isEmpty) {
          println("g.labelMap: "+g.labelMap(q.labels(u)))
          println("u: " + u)
          println("v: " + v)
          println("w: " + w)
          println("g.pre(w): " + g.pre(w))
          println("sim(v): " + sim(v))
          return HashMap[Int, Set[Int]]()
        }
        flag = true
      }
    }

    sim
  }


  def filterGraph(g: Graph, q: Graph,  sim: HashMap[Int, Set[Int]]): Graph = {
    var nodesInSimset = sim.values.flatten.toSet
    for (i <- 0 until g.adjSet.size) {
      g.adjSet(i) &= nodesInSimset
      g.parList(i) &= nodesInSimset
    }
    var newAdjSet = Array.ofDim[Set[Int]](g.adjSet.size)
    var newParList = Array.ofDim[Set[Int]](g.parList.size)
    for (i <- 0 until newAdjSet.size) {
      newAdjSet(i) = Set[Int]()
      newParList(i) = Set[Int]()
    }
    for (u <- 0 until q.n; w <- sim(u)){
      for (v <- q.post(u))
        newAdjSet(w) |= (g.post(w) & sim(v))
      for (v <- q.pre(u))
        newParList(w) |= (g.pre(w) & sim(v))
    }
    g.adjSet = newAdjSet
    g.parList = newParList
  
    g
  }

  def dualFilter(query: Graph, sim: HashMap[Int, Set[Int]], ball: Ball): HashMap[Int, Set[Int]] = {

    // project simset onto ball
    for (v <- sim.keySet) sim(v) &= ball.nodesInBall

    val filterSet = new Stack[(Int, Int)]()
    var filtered = false
    for ((u, simU) <- sim.iterator; v <- simU if ball.borderNodes.contains(v)) {
      filtered = false
      breakable {
        for (u1 <- query.post(u)) {
          if ((ball.post(v) & sim(u1)).isEmpty) {
            //            println("filtering "+ v +" for children")
            filterSet.push((u, v))
            filtered = true
            break
          }
        }
      }
      if (!filtered) {
        breakable {
          for (u2 <- query.pre(u)) {
            if ((ball.pre(v) & sim(u2)).isEmpty) {
              //              println("filtering "+ v +" for parents")
              filterSet.push((u, v))
              break
            }
          }
        }
      }
    }

    while (!filterSet.isEmpty) {
      val (u, v) = filterSet.pop()
      sim(u) -= v
      for (u2 <- query.pre(u); v2 <- (ball.pre(v) & sim(u2)) if (ball.post(v2) & sim(u)).isEmpty) {
        filterSet.push((u2, v2))
      }

      for (u1 <- query.post(u); v1 <- (ball.post(v) & sim(u1)) if ((ball.pre(v1) & sim(u)).isEmpty)) {
        filterSet.push((u1, v1))
      }
    }

    var adjSet = HashMap[Int, mutable.Set[Int]]()
    var parList = HashMap[Int, mutable.Set[Int]]()

    for ((u, uSim) <- sim.iterator) {
      for (v <- uSim) {
        for (uc <- query.post(u)) {
          for (vc <- (ball.post(v) & sim(uc))) {
            adjSet.getOrElseUpdate(v, Set[Int]()) += vc
            parList.getOrElseUpdate(vc, Set[Int]()) += v
          }
        }
      }
    }

    // Finding max perfect subgraph
    var stack = new Stack[Int]()
    var visited = Set(ball.ballcenter)
    stack.push(ball.ballcenter)
    while (!stack.isEmpty) {
      var v = stack.pop()
      for (child <- (adjSet.getOrElse(v, Set()) | parList.getOrElse(v, Set()))) {
        if (!visited.contains(child)) {
          stack.push(child)
          visited += child
        }
      }
    }
    for ((v, simV) <- sim.iterator) {
      sim(v) = simV & visited
    }

    //fixes the edges in the ball
    //(note that it does not change the parent set; this is only used for printing)
    //uncomment if you want to see the ball after finding maximum perfect subgraph
    if(printSetting.trim.equals("0")){
      ball.adjSet = Map[Int, Set[Int]]()
      val matchNodes = sim.values.flatten.toSet
      for ((n, nset) <- adjSet; nc <- nset){
        if ((matchNodes contains n) && (matchNodes contains nc)) ball.adjSet.getOrElseUpdate(n, Set()) += nc
      }
    }

    for ((v, simV) <- sim.iterator if simV.isEmpty) {
      return HashMap[Int, mutable.Set[Int]]()
    }

    sim
  }
 
  def printDualSimMatch(sim: HashMap[Int, Set[Int]]) {
    println("dualSim match:")
    for ((u, v) <- sim) {
      println(u + "  " + v)
    }
    println("-------------")
  }

  def printArash(center: Int, mat: HashMap[Int, Set[Int]]) {
    println("\n\nMatch for ball centered at " + center + ": ")
    for ((u, v) <- mat.iterator) {
      println(u + "  " + v)
    }
    println("-----------")
  }

  def printUsman(g: Graph, balls: HashMap[Int, Ball], matchCenters: Set[Int]) {
    for (vert_id <- 0 until g.adjSet.length) {
      var ballstring = ""
      var isMatch = 0
      if (balls.keySet.contains(vert_id)) {
        ballstring = balls.get(vert_id).get.getBallAsString()
        if (matchCenters contains vert_id) isMatch = 1
      }
      println(vert_id + " " + g.labels(vert_id) + " " + ballstring + " " + isMatch)
    }
  }

  def displaySimSet(q: Graph, sim: HashMap[Int, Set[Int]]) {
    for (v <- 0 until q.n) println(v + " " + sim(v))
  }

  def time[R](block: => R): (R, Double) = {
    val t0 = System.currentTimeMillis()
    val result = block // call-by-name
    val t1 = System.currentTimeMillis()
    (result, (t1 - t0))
  }
}
