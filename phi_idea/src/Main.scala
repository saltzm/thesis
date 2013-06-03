import scala.io._
import scala.collection.{mutable => mut}

object TestVsDual extends App {
  val (qAdjList, qLabels) = Main.readQuery(args(0), args(1)) 
  val (gAdjList, gLabelIndex) = Main.readGraph(args(2), args(3))
  val phi = Array.ofDim[Set[Int]](4)
  phi(0) = Set(0, 5, 11)
  phi(1) = Set(1, 10, 4)
  phi(2) = Set(9, 2, 6)
  phi(3) = Set(3, 7, 8)
  //println(Main.saltzism(phi, qAdjList, gAdjList).deep.mkString("\n"))
  var f = List[Array[Set[Int]]]() 
/*  for (i <- phi(0); j <- phi(1); k <- phi(2); l <- phi(3)) {*/
    //f = Main.saltzism(Array(Set(i), Set(j), Set(k), Set(l)), qAdjList, gAdjList) :: f
  /*}*/
  // if you confine phi(0) to one result at a time


  // TODO: question: can a dual simulation result exist, that is not an isomorphism,
  // with only one node in some phi(i), 0 < i < query_size?
  // TODO: answer:   yes (example g1 and q1 in scalagraphsim directory)
  // TODO: question:  must there exist some query node n such that, if phi(n) is
  // restricted to only one node, and dual simulation is performed, the remaining
  // must be isomorphic
  // TODO: reworded hypothesis:  For every dual simulation result set r, there exists
  // some query node n such that, if dual simulation is run on r such that phi(n)
  // is constrained to one node in phi_r(n), the results of these dual simulations
  // must be isomorphisms.
  // If this hypothesis is true, then we can repeat dual simulation on the result set
  // q_size * g_size in the worst case, where each time restricts phi(i) to a different
  // node in the result set of the initial dual simulation. Whenever phi(i) is found
  // such that no sub_simulation returns a result, it is guaranteed that there are
  // no isomorphisms in the result set.
  // Another aspect of the algorithm:  In the case that a subgraph isomorphism is 
  // contained in a dual result, checking that no such phi exists where the singulation
  // of its elements all cause empty result sets does not imply that the remaining
  // results are isomorphic.  however: TODO: if you remove any element e of phi(i) which
  // creates an empty result set on its own, then you can remove that from phi(i)
  // TODO: however, i think it is necessary at that point to reiterate over previous
  // phi's that have been deemed acceptable.  This occurs a max of O(g_size) times
  // (and that could be way overestimating).
  for (j <- phi(3)) {
    f = Main.saltzism(Array(phi(0), phi(1), phi(2), Set(j)), qAdjList, gAdjList) :: f
  }
  f.map {
    x => println(x.deep.mkString(", ")); println
  }
}

object Main extends App {
  if (args.length != 4) {
    println("Usage:  <q_label_file> <q_edge_file> <g_label_file> <g_edge_file>")
  }
  // read in query and get distinct edge labelings from query
  val (qAdjList, qLabels) = readQuery(args(0), args(1)) 
  println("qLabels")
  qLabels.foreach{case (x,y) => println(x +" -> " + y.deep.mkString(","))}
  println("qAdjList")
  qAdjList.foreach{ x => println(x)}
  println
  // read in data graph and build index on data graph
  val (gAdjList, gLabelIndex) = readGraph(args(2), args(3))
  println("gLabelIndex")
  println(gLabelIndex)
  println("gAdjList")
  gAdjList.foreach{ x => println(x)}
  println

  // for each node in the query graph // TODO: do we need them all?
  //     find edge sets in the data index that match the edge labelings
  //     going out of this node
  //     intersect these sets based on the "from node" and save the candidate
  //     edges somewhere 
  //     
  // for each key in the index which matches a labeling of a query edge
  //     extract 

  def saltzism(initPhi: Array[Set[Int]], 
               qAdjList: Array[Set[Int]],
               gAdjList: Array[Set[Int]]): Array[Set[Int]] = {
    val phi = initPhi 
    // TODO: must calculate feasible mates

    // TODO: dumb idea... check at the end every combo of phi(0)...phi(n)
    // because an isomorphic match is a bijection

    // TODO: problem: could potentially allow phiNodes which have the same
    // child being used for multiple query nodes
    var changed = true
    while (changed) {
      changed = false
      for (qNode <- 0 until qAdjList.size) {
        for (qChild <- qAdjList(qNode)) {
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
            val phiTemp = gAdjList(phiNode) & phi(qChild)
            if (phiTemp.isEmpty) phi(qNode) -= phiNode 
            newPhi = newPhi union phiTemp
          }
          // if any phi(i) is empty, then there is no 
          // isomorphic subgraph.
          if (newPhi.isEmpty) return Array[Set[Int]]() 
          if (phi(qChild).size > newPhi.size) changed = true
          // TODO: would intersect be faster? (doubt it)
          // every node in phi(qChild) must have at least one parent
          // in phi(qNode)
          phi(qChild) = newPhi 
        }
      }
    }
    phi
  }

  /**
  * lFile:  File with labels
  * eFile:  File with edges
  */
  def readQuery(lFile: String, eFile: String):
               (Array[Set[Int]], Array[(Int, Array[Int])]) = {

    val labels = readLabels(lFile)
    // assumes node per line, each item on line is child of
    // that node
    val labelArr = labels.zip(Source.fromFile(eFile).getLines.toArray.map( 
      line => 
        if( !(line.trim equals ""))
          line.split(" ").map( x => labels(x.trim.toInt) )
        else Array[Int]()
    ))
    val adjList = Source.fromFile(eFile).getLines.toArray.map(
      line =>
        if(!(line.trim equals ""))
          line.split(" ").map( x => x.trim.toInt ).toSet
        else Set[Int]()
    )
    (adjList, labelArr)
  }

  // map from (label1, label2) to (node1, node2)'s
  // TODO: make set mutable?
  def readGraph(lFile: String, eFile: String): 
               (Array[Set[Int]], Map[(Int, Int), List[Int]]) = {

    val labels = readLabels(lFile)
    // TODO: faster to be mutable?
    var map = Map[(Int, Int), List[Int]]()
    var eLines = Source.fromFile(eFile).getLines.toArray
    // i is the current node id
    for( i <- 0 until labels.size ) {
      var eLine = eLines(i)
      if (!(eLine.trim equals "")) {
        val children = eLine.split(" ").map(_.trim.toInt)
        children.foreach { c =>
          // check if any edges have had labeling curLabel -> childLabel,
          // and if so, add the "from" node i to the set in the map
          val edgeLabel = (labels(i), labels(c))
          val curEdgeLabelList = map.getOrElse(edgeLabel, List[Int]())
          val newList = i :: curEdgeLabelList
          map += edgeLabel -> newList
        }
      } else {
        // really hacky.  this means i is a leaf node.
        val edgeLabel = (labels(i), -1)
        val curEdgeLabelList = map.getOrElse(edgeLabel, List[Int]())
        val newList = i :: curEdgeLabelList 
        map += edgeLabel -> newList
      }
    }
    val adjList = eLines.map(
      line =>
        if(!(line.trim equals ""))
          line.split(" ").map( x => x.trim.toInt ).toSet
        else Set[Int]()
    )

    (adjList, map)
  }

 
  /** 
  *  TODO: toArray inefficient?
  *  assumes node labels 0 to l, one label per line
  */ 
  def readLabels(lFile: String) = 
    Source.fromFile(lFile).getLines.toArray.map(_.toInt)
}
