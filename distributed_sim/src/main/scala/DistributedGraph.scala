import akka.actor._
import scala.io._
import akka.remote._
import com.typesafe.config.ConfigFactory

case class GraphSuccessfullyLoaded
case class PartitionAdjacencyRequest(id: Int, requestor: ActorRef)
case class PartitionLabelRequest(id: Int, requestor: ActorRef)
case class AdjacencyRequest(id: Int)
case class LabelRequest(id: Int)
case class AdjNodeSet(id: Int, set: Set[Int])
case class LabelNodeSet(label: Int, set: Set[Int])
case class NodeLabel(id: Int, label: Int)
case class LabelIndexRequest(label: Int)

class DistributedGraph(nPartitions: Int, labelFile: String, 
                       edgeFile: String) 
  extends Actor with ActorLogging{
 
  var partitions: Array[ActorRef] = _ 
  var labelIndex: ActorRef = _
  var nodesPerPartition: Int = _
  var size: Int = _

  def receive = {
    case AdjacencyRequest(nodeId) => 
      findPartition(nodeId) ! PartitionAdjacencyRequest(nodeId, sender)

    case LabelRequest(nodeId) =>
      findPartition(nodeId) ! PartitionLabelRequest(nodeId, sender)

    case LabelIndexRequest(l) =>
      labelIndex ! DistributedLabelIndexRequest(l, sender)

    case _ => 
      println("Invalid request to DistributedGraph!") 
      context.system.shutdown()
  }

  def findPartition(nodeId: Int) = partitions(nodeId/nodesPerPartition)

  override def preStart() = {
    size = Source.fromFile(labelFile).getLines.size
    partitions = Array.ofDim[ActorRef](nPartitions)
    nodesPerPartition = math.ceil(size/nPartitions.toDouble).toInt
    labelIndex = context.actorOf(Props[DistributedLabelIndex])
    val edgeIter = Source.fromFile(edgeFile).getLines
    val labelIter = Source.fromFile(labelFile).getLines
    var nodeCounter = 0

    for (p <- 0 until nPartitions) {   
      val partSize = math.min(nodesPerPartition, size - p*nodesPerPartition).toInt
      val adjList = Array.ofDim[Set[Int]](partSize)
      val labels = Array.ofDim[Int](partSize)
      for (i <- 0 until partSize) {
        val line = edgeIter.next 
        adjList(i) = 
          if(!(line.trim equals "")) line.split(" ").map(_.trim.toInt).toSet
          else Set[Int]()
        labels(i) = labelIter.next.trim.toInt
        labelIndex ! AddNodeToLabel(labels(i), nodeCounter)
        nodeCounter += 1
      }
      partitions(p) = context.actorOf(Props(
        new GraphPartition(p, nodesPerPartition, adjList, labels)
      ), s"graphPartition$p")
    }
    println("finished starting up distributed graph")
    context.parent ! GraphSuccessfullyLoaded
    if(!edgeIter.isEmpty) {
      println("LOGIC ERROR!")
      context.system.shutdown()
      System.exit(-1)
    }
  }

  def buildLabelMapFromLabels(labels: Array[Int]): 
                              Map[Int, Set[Int]] = {
    var labelMap = Map[Int, Set[Int]]()
    labels.foldLeft(0) ( (i, label) => { 
      labelMap = labelMap + (label -> (labelMap.getOrElse(label, Set[Int]()) + i))
      i + 1 
    })
    labelMap 
  }

}

class GraphPartition(partitionId: Int, nodesPerPartition :Int,
                     adjList: Array[Set[Int]], labels: Array[Int]) 
  extends Actor with ActorLogging {

  def receive = {

    case PartitionAdjacencyRequest(nodeId, requestor) => 
      requestor ! AdjNodeSet(nodeId, adjList(convertNodeId(nodeId)))
      
    case PartitionLabelRequest(nodeId, requestor) =>
      requestor ! NodeLabel(nodeId, labels(convertNodeId(nodeId)))

    
    case _ => log.error("Invalid request to GraphPartition!") 
  }

  def convertNodeId(nodeId: Int) = nodeId - partitionId * nodesPerPartition 
  def convertToRealId(thisId: Int) = thisId + partitionId * nodesPerPartition 
}

object DistGraphInvalidMessageTest extends App {
  val system = ActorSystem("distGraphIso")
  val g = GraphGenerator.generateRandomGraph(1000, 100, 6)
  g.writeToFiles("dgl.txt", "dge.txt")
  val d = system.actorOf(Props(new DistributedGraph(3, "dgl.txt", "dge.txt")))
  d ! AddNode(3)
}

object DistGraphGetNodeTest extends App {
  val system = ActorSystem("DistGraphIso")//, 
    //ConfigFactory.load.getConfig("remotecreation"))
  val g = GraphGenerator.generateRandomGraph(100000, 5, 6)
  g.writeToFiles("dgl.txt", "dge.txt")
  case class RequestNode(id: Int)
  case class RequestLabel(id: Int)
  case class RequestLabelSet(id: Int)
  class Listener(distGraph: ActorRef) extends Actor {
    val startTime = System.nanoTime()
    def receive = { 
      case RequestNode(id) =>
        distGraph ! AdjacencyRequest(id)

      case RequestLabel(id) =>
        distGraph ! LabelRequest(id)

      case RequestLabelSet(label) =>
        distGraph ! LabelIndexRequest(label)

      case AdjNodeSet(id, set) => 
        //println("NODE ID: " + id + ": " + set)

      case NodeLabel(id, label) => 
        //println("NODE LABEL: " + id + ": " + label)

      case LabelNodeSet(label, set) => 
        //println("LABEL NODE SET: " + label + ": " + set)
        println("Total Time: " + (System.nanoTime - startTime)/1000000.0)
        context.system.shutdown()
    }
  }
  val d = system.actorOf(Props(new DistributedGraph(4, "dgl.txt", "dge.txt")),
                         "distGraph")
  Thread.sleep(10000)
  val l = system.actorOf(Props(new Listener(d)))
  for (i <- 0 until g.size) {
    l ! RequestNode(i)
  }
  for (i <- 0 until g.size) {
    l ! RequestLabel(i)
  }
  for (label <- g.labelMap.keys) {
    l ! RequestLabelSet(label)
  }
  
}

