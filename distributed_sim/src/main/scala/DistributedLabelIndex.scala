import akka.actor._

case class AddNode(id: Int)
case class AddNodeToLabel(label: Int, id: Int)
case class AddNodes(nodes: Set[Int])
case class AddNodesToLabel(label: Int, nodes: Set[Int])
case class DistributedLabelIndexRequest(label: Int, requestor: ActorRef)
case class PartitionLabelIndexRequest(requestor: ActorRef)

class DistributedLabelIndex extends Actor {
  // TODO: for now, just having an actor for each label
  //var labelMap = Map[Int, ActorRef]()
  var labelMap = Map[Int, Set[Int]]()

  def receive = { 

    case AddNodeToLabel(label, id) => 
      if (labelMap.contains(label))
        labelMap += (label -> (labelMap(label) + id))
        //labelMap(label) ! AddNode(id)
      else {
        labelMap += (label -> Set(id)) /*context.actorOf(Props(*/
          //new LabelPartition(label)), s"labelIndex$label")
        /*)*/
        //labelMap(label) ! AddNode(id)
      }

/*    case AddNodesToLabel(label, nodes) =>*/
      //if (labelMap.contains(label))

        ////labelMap(label) ! AddNodes(nodes)
      //else {
        //labelMap += (label -> context.actorOf(Props(new LabelPartition(label))))
        //labelMap(label) ! AddNodes(nodes)
      /*}*/

    case DistributedLabelIndexRequest(label, requestor) =>
 /*     if (labelMap.contains(label))*/
        //labelMap(label) ! PartitionLabelIndexRequest(requestor)
      /*else*/
        requestor ! LabelNodeSet(label, labelMap.getOrElse(label, Set[Int]()))

  }
}

class LabelPartition(label: Int) extends Actor {
  var nodes = Set[Int]()

  def receive = {
    case AddNode(id) => nodes += id

    // not being used right now
    case AddNodes(set) => nodes = nodes union set

    case PartitionLabelIndexRequest(requestor) => 
      requestor ! LabelNodeSet(label, nodes)

    case _ => 
      println("Invalid message to LabelPartition!")
      context.system.shutdown()
  }
}
