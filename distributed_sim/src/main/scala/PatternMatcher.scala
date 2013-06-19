import java.io._
import scala.collection.{mutable => mut}
import akka.actor._
import com.typesafe.config.ConfigFactory

case class IsoResults(matches: Set[Array[Set[Int]]])
case class Start

class PatternMatcher(resultHandler: ActorRef,
  distGraph: ActorRef, q: Graph) extends Actor {

  val initPhi = Array.ofDim[Set[Int]](q.size) 
  var flatPhi: Set[Int] = _
  var gAdjList = Map[Int, Set[Int]]()
  var receivedPhis = 0
  var receivedAdjs = 0
  var startTime = 0L
  var t1 = 0L
  var t2 = 0L

  def receive = {
    case Start =>
      startTime = System.nanoTime()
      requestFeasibleMates
   
    // For each node in q with this label, nodes represents the feasible
    // mates for that node  TODO: might not need to clone
    case LabelNodeSet(label, nodes) => 
      if (nodes.isEmpty) { 
        resultHandler ! IsoResults(Set())
        context.stop(self)
      } else q.labelMap(label).foreach{ 
        node => initPhi(node) = nodes.map(x=>x) 
        receivedPhis += 1
      }
      if (receivedPhis == q.size) {
        flatPhi = initPhi.flatten.toSet
        flatPhi.foreach( node => distGraph ! AdjacencyRequest(node) )
        t1 = System.nanoTime()
        println("Stage 1 Time: " + (t1 - startTime)/1000000.0)
      }

//TODO:  look for ways to make this faster, figure out remoting
    case AdjNodeSet(id, set) => 
      // fills a local adjSet for g
      //TODO: faster if i leave out the intersection, but more mem.
      gAdjList += (id -> (set & flatPhi)) // TODO: do something similar for centralized
      receivedAdjs += 1
      if (receivedAdjs == flatPhi.size) { 
        t2 = System.nanoTime()
        println("Stage 2 Time: " + (t2 - t1)/1000000.0)
        val matches = saltzIso3
        resultHandler ! IsoResults(matches)
        val t3 = System.nanoTime()
        println("Iso time: " + (t3 - t2)/1000000.0) 
        println("Total time: " + (t3 - startTime)/1000000.0)
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
    saltzIso_(saltzDualSim(initPhi), 0) 
  
    def saltzIso_(phi: Array[Set[Int]], depth: Int) {
      if (depth == q.size) {
        matches += phi
        if (matches.size == 1024) {
          println("First 1024 time: " + 
            (System.nanoTime - t2)/1000000.0)  
        }
      }
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
  def prepGraphs: Graph = { 
    val g = GraphGenerator.generateRandomGraph(10000, 1000, 16)
    g.writeToFiles("dgl.txt", "dge.txt")
    g.writeToFile("dg.txt")
    println("Finished generating graph")
    println("Generating query")
    val q = GraphGenerator.generateBFSQuery(4, 2, g)
    q.writeToFile("dq.txt")
    q
  }
  val q = prepGraphs
  val masterSystem = ActorSystem("DistGraphIso", 
    ConfigFactory.load.getConfig("graphiso"))
   
  println("spawning master")
  val master = masterSystem.actorOf(
    Props[Master].withDispatcher("mydispatcher"), "master"
  )
  master ! SubmitQuery(q)
}

case class SubmitQuery(q: Graph)

class Master extends Actor with Stash {
  import context._

  var g: ActorRef = _

  override def preStart() = {
    g = context.actorOf(Props(
      new DistributedGraph(16, "dgl.txt", "dge.txt")), "distGraph"
    )
  }

  def receive = loading
  
  def loading: Receive = {
    case GraphSuccessfullyLoaded => 
      println("Master: Graph successfully loaded")
      become(ready)
      unstashAll()
    case _ => stash() 
  }

  def ready : Receive = {
    case SubmitQuery(q) =>
      println("Starting query")
      val p = context.actorOf(Props(new PatternMatcher(self, g, q))) 
      p ! Start
      
    case IsoResults(matches) =>
      println("Number of matches: " + matches.size)
      println("Number of unique nodes: " + matches.flatten.toSet.size)
      if (matches.isEmpty) println("no matches")
      context.system.shutdown()
  }
}

