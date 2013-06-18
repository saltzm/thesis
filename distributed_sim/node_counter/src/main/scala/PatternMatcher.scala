import java.io._
import scala.collection.{mutable => mut}
import akka.actor._

case class IsoResults(matches: Set[Array[Set[Int]]])

class PatternMatcher(resultHandler: ActorRef,
  distGraph: ActorRef, q: Graph) extends Actor {

  val initPhi = Array.ofDim[Set[Int]](q.size) 
  var flatPhi: Set[Int] = _
  var gAdjList = Map[Int, Set[Int]]()
  var receivedPhis = 0
  var receivedAdjs = 0

  val startTime = System.nanoTime()

  requestFeasibleMates
  
  def receive = {
    
    // For each node in q with this label, nodes represents the feasible
    // mates for that node  TODO: might not need to clone
    case LabelNodeSet(label, nodes) => 
      if (nodes.isEmpty) { 
        resultHandler ! IsoResults(Set()) // TODO: shutdown actor
        context.stop(self)
      }
      else q.labelMap(label).foreach{ 
        node => initPhi(node) = nodes.map(x=>x) 
        receivedPhis += 1
      }
      if (receivedPhis == q.size) {
        flatPhi = initPhi.flatten.toSet
        flatPhi.foreach( node => distGraph ! AdjacencyRequest(node) )
      }
    
    case AdjNodeSet(id, set) => 
      // fills a local adjSet for g
      gAdjList += (id -> (set & flatPhi)) // TODO: do something similar for centralized
      receivedAdjs += 1
      if (receivedAdjs == flatPhi.size) { 
        val matches = saltzIso3
        resultHandler ! IsoResults(matches)
        println("Time: " + (System.nanoTime() - startTime)/1000000.0)
        context.stop(self)
      }
    case _ => 
      println("Invalid message to PatternMatcher!")
      context.system.shutdown()
  }

  def requestFeasibleMates {
    // TODO:  WTF IS UP WITH THIS INT DECLARATION
    for (qNodeLabel:Int <- q.labels.toSet) 
      distGraph ! LabelIndexRequest(qNodeLabel)
  }
  

  def saltzIso3: Set[Array[Set[Int]]] = {
    var matches = Set[Array[Set[Int]]]()
    saltzIso_(initPhi, 0) 
  
    def saltzIso_(phi: Array[Set[Int]], depth: Int) {
      if (depth == q.size) matches += phi
      else if (!phi.isEmpty) {
        for (i <- phi(depth) if (!contains(phi, i, depth))) {
          val phiCopy = phi.map(x=>x)
          phiCopy(depth) = Set[Int](i)
          saltzIso_(saltzDualSim(phiCopy), depth + 1)
        }
      }
    }

    def contains(phi: Array[Set[Int]], ele: Int, depth: Int): Boolean = {
      for (i <- 0 until depth) if (phi(i) contains ele) return true
      false 
    }
    matches 
  }

  def saltzDualSim(phi: Array[Set[Int]]): Array[Set[Int]] = {
    var changed = true
    while (changed) {
      changed = false
      for (qNode <- 0 until q.size) {
        for (qChild <- q.adjList(qNode)) {
          var newPhi = Set[Int]()
          for (phiNode <- phi(qNode)) {
            val phiTemp = gAdjList(phiNode) & phi(qChild)
            if (phiTemp.isEmpty) { 
              phi(qNode) -= phiNode 
              if (phi(qNode).isEmpty) return Array[Set[Int]]()
              changed = true  
            }
            newPhi = newPhi union phiTemp
          }
          if (newPhi.isEmpty) return Array[Set[Int]]() 
          if (phi(qChild).size > newPhi.size) changed = true
          phi(qChild) = newPhi 
        }
      }
    }
    phi
  }
 
  
  def writePhiToFile(phi: Array[Set[Int]], fileName: String) {
    val out = new PrintWriter(fileName)
    phi.foreach { p => out.println(p) }
    out.close
  }
}

object PatMatchTest extends App {
  val system = ActorSystem("DistGraphIso")
  val g = GraphGenerator.generateRandomGraph(100, 5, 6)
  g.writeToFile("dg.txt")
  val d = system.actorOf(Props(new DistributedGraph(4, "dgl.txt", "dge.txt")),
                         "distGraph")
  val q = GraphGenerator.generateBFSQuery(4, 2, g)
  q.writeToFile("dq.txt")
  class PrintResultHandler extends Actor {
    def receive = {
      case IsoResults(matches) =>
        println("Number of matches: " + matches.size)
        println("Number of unique nodes: " + matches.flatten.toSet.size)
        if (matches.isEmpty) println("no matches")
        context.system.shutdown()
    }
  }

  val resHandler = system.actorOf(Props[PrintResultHandler])
  println("spawning pat matcher")
  val p = system.actorOf(Props(new PatternMatcher(resHandler, d, q)))
}
