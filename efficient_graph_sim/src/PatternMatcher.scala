import java.io._
import scala.collection.{mutable => mut}
import scala.actors._

case class Start()
case class End()
case class Match(m: Array[Set[Int]])
case class PrintNumberOfMatches()
case class Exit()
case class MatchCollector() extends Actor {
  var matches = Set[Array[Set[Int]]]()
  def act() = {
    loop {
      receive { 
      case Match(m: Array[Set[Int]]) => 
        matches += m
      case PrintNumberOfMatches() =>
        sender ! matches.size
      case Exit() => 
        exit
      }
    }
  }
}
case class Tracker(mc: Actor) extends Actor {
  def act() = {
    var activeActors = 0
    val startTime = System.nanoTime()
    loop {
      receive {
        case Start() => { activeActors += 1/*; println("START: " + activeActors); */}
        case End() => { 
          activeActors -= 1 
          //println("END: " + activeActors)
          if (activeActors == 0) {
            val nMatches = mc !? PrintNumberOfMatches 
            println("Number of Matches: " + nMatches)
            val endTime = System.nanoTime()
            println("Parallel Iso Took: " + (endTime - startTime)/1000000.0 + " ms")
            System.exit(0) 
          }
        }
      }
    }
  }
}
case class IsoHelper(g: Graph, q: Graph, phi: Array[Set[Int]], depth: Int, 
                     matchCollector: Actor, tracker: Actor) extends Actor {
  def act() = { 
    if (depth == q.size) { 
      matchCollector ! Match(phi)
      tracker ! End
    }
    else if (!phi.isEmpty) {
      for (i <- phi(depth) if (!contains(phi, i, depth))) {
        val phiCopy = phi.map(x=>x)
        phiCopy(depth) = Set[Int](i)
        if (depth == q.size - 1) tracker ! Start
        IsoHelper(g, q, PatternMatcher.saltzDualSim(g, q, phiCopy), depth + 1, 
                  matchCollector, tracker).start()
      }
    } else { tracker ! End }
  }
  def contains(phi: Array[Set[Int]], ele: Int, depth: Int): Boolean = {
    for (i <- 0 until depth) if (phi(i) contains ele) return true
    false 
  }
}

object PatternMatcher {

  def ullmannDual(g: Graph, q: Graph): Set[Array[Set[Int]]] = {
    var matches = Set[Array[Set[Int]]]()
    var initPhi = saltzDualSim(g, q)
    ullmann_(g, q, initPhi, 0) 
  
    def ullmann_(g: Graph, q: Graph, phi: Array[Set[Int]], depth: Int) {
      if (depth == q.size) matches += phi
      else if (!phi.isEmpty) {
        for (i <- phi(depth) if (!contains(phi, i, depth))) {
          val phiCopy = phi.map(x=>x)
          phiCopy(depth) = Set[Int](i)
          ullmann_(g, q, refine(g, q, phiCopy), depth + 1)
        }
      }
    }

    def contains(phi: Array[Set[Int]], ele: Int, depth: Int): Boolean = {
      for (i <- 0 until depth) if (phi(i) contains ele) return true
      false 
    }
    matches 
  }


  def ullmann(g: Graph, q: Graph): Set[Array[Set[Int]]] = {
    var matches = Set[Array[Set[Int]]]()
    var initPhi = feasibleMates(g, q)
    ullmann_(g, q, initPhi, 0) 
  
    def ullmann_(g: Graph, q: Graph, phi: Array[Set[Int]], depth: Int) {
      if (depth == q.size) matches += phi
      else if (!phi.isEmpty) {
        for (i <- phi(depth) if (!contains(phi, i, depth))) {
          val phiCopy = phi.map(x=>x)
          phiCopy(depth) = Set[Int](i)
          ullmann_(g, q, refine(g, q, phiCopy), depth + 1)
        }
      }
    }

    def contains(phi: Array[Set[Int]], ele: Int, depth: Int): Boolean = {
      for (i <- 0 until depth) if (phi(i) contains ele) return true
      false 
    }
    matches 
  }

  def refine(g: Graph, q: Graph, phi: Array[Set[Int]]): Array[Set[Int]] = {
    for (qNode <- 0 until q.size) {
      for (qChild <- q.adjList(qNode)) {
        var newPhi = Set[Int]()
        for (phiNode <- phi(qNode)) {
          val phiTemp = g.adjList(phiNode) & phi(qChild)
          if (phiTemp.isEmpty) { 
            phi(qNode) -= phiNode 
          }
          newPhi = newPhi union phiTemp
        }
        if (newPhi.isEmpty) return Array[Set[Int]]() 
        phi(qChild) = newPhi 
      }
    }
    phi
  }

  def saltzIso3(g: Graph, q: Graph): Set[Array[Set[Int]]] = {
    var matches = Set[Array[Set[Int]]]()
    var initPhi = saltzDualSim(g, q, feasibleMates(g, q))
    saltzIso_(g, q, initPhi, 0) 
  
    def saltzIso_(g: Graph, q: Graph, phi: Array[Set[Int]], depth: Int) {
      if (depth == q.size) matches += phi
      else if (!phi.isEmpty) {
        for (i <- phi(depth) if (!contains(phi, i, depth))) {
          val phiCopy = phi.map(x=>x)
          phiCopy(depth) = Set[Int](i)
          saltzIso_(g, q, saltzDualSim(g, q, phiCopy), depth + 1)
        }
      }
    }

    def contains(phi: Array[Set[Int]], ele: Int, depth: Int): Boolean = {
      for (i <- 0 until depth) if (phi(i) contains ele) return true
      false 
    }
    matches 
  }

  // TODO:  Doesn't work.  Figure out a better way to calculate when to end,
  // do pooling, etc.
  def saltzIsoParallel(g: Graph, q: Graph)  {
    var matchC = MatchCollector().start() 
    val tracker = Tracker(matchC).start()
    var initPhi = saltzDualSim(g, q, feasibleMates(g, q))
    IsoHelper(g, q, initPhi, 0, matchC, tracker).start()
  }

  def saltzIso4(g: Graph, q: Graph): Set[Array[Set[Int]]] = {
    var matches = Set[Array[Set[Int]]]()
    var initPhi = saltzDualSim(g, q, feasibleMates(g, q))
    saltzIso_(g, q, initPhi, 
              initPhi.zipWithIndex.sortBy(_._1.size).map(_._2), 
              0) 
  
    def saltzIso_(g: Graph, q: Graph, phi: Array[Set[Int]], 
                  ordering: Array[Int], depth: Int) {
      if (depth == q.size) matches += phi
      else if (!phi.isEmpty) {
        for (i <- phi(ordering(depth)) if (!contains(phi, i, ordering, depth))) {
          val phiCopy = phi.map(x=>x)
          phiCopy(ordering(depth)) = Set[Int](i)
          saltzIso_(g, q, saltzDualSim(g, q, phiCopy), ordering, depth + 1)
        }
      }
    }

    def contains(phi: Array[Set[Int]], ele: Int, ordering: Array[Int], 
                 depth: Int): Boolean = {
      for (i <- 0 until depth) if (phi(ordering(i)) contains ele) return true
      false 
    }
    matches 
  }

  def saltzDualSim(g: Graph, q: Graph): Array[Set[Int]] = 
    saltzDualSim(g, q, feasibleMates(g, q))

  def saltzDualSim(g: Graph, q: Graph, phi: Array[Set[Int]]): Array[Set[Int]] = {
    var changed = true
    while (changed) {
      changed = false
      for (qNode <- 0 until q.size) {
        for (qChild <- q.adjList(qNode)) {
          // newPhi corresponds to phi(qChild).  This update
          // will ensure that phi(qChild) will contain only 
          // nodes which have a parent in phi(qNode)
          var newPhi = Set[Int]()
          for (phiNode <- phi(qNode)) {
            // phiTemp corresponds to the children of 
            // phiNode which are contained in phi(qChild).
            // This checks both if phiNode has children in
            // phi(qChild) (of which it must have at least one)
            // and also builds newPhi to contain only those
            // nodes in phi(qChild) which also have a parent
            // in phi(qNode)
            val phiTemp = g.adjList(phiNode) & phi(qChild)
            if (phiTemp.isEmpty) { 
              phi(qNode) -= phiNode 
              if (phi(qNode).isEmpty) return Array[Set[Int]]()
              changed = true  
            }
            newPhi = newPhi union phiTemp
          }
          // if any phi(i) is empty, then there is no 
          // isomorphic subgraph.
          if (newPhi.isEmpty) return Array[Set[Int]]() 
          if (phi(qChild).size > newPhi.size) changed = true
          // every node in phi(qChild) must have at least one parent
          // in phi(qNode)
          phi(qChild) = newPhi 
        }
      }
    }
    phi
  }
 
  def feasibleMates(g: Graph, q: Graph): Array[Set[Int]] = 
    q.labels.map(qNodeLabel => g.labelMap(qNodeLabel)) //TODO: need to clone?

  def writePhiToFile(phi: Array[Set[Int]], fileName: String) {
    val out = new PrintWriter(fileName)
    phi.foreach { p => out.println(p) }
    out.close
  }
}
