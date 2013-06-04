import java.io._

object PatternMatcher { 

  def saltzIso(g: Graph, q: Graph): Set[Array[Set[Int]]] = {
    var initPhi = saltzDualSim(g, q, feasibleMates(g, q))
    saltzIso(g, q, initPhi, 0) 
  }

  def saltzIso(g: Graph, q: Graph, phi: Array[Set[Int]], depth: Int): 
               Set[Array[Set[Int]]] = {
    if (depth == q.size) Set(phi)
    else if (phi.isEmpty) Set[Array[Set[Int]]]()
    else {
      var matches = Set[Array[Set[Int]]]()
      for (i <- phi(depth) if (!phi.slice(0, depth).flatten.contains(i))) {
        val phiCopy = phi.map(x=>x.map(y=>y))
        phiCopy(depth) = Set[Int](i)
        matches = matches ++ saltzIso(g, q, saltzDualSim(g, q, phiCopy), depth + 1)
      }
      matches
    }
  }

  def saltzDualSim(g: Graph, q: Graph): Array[Set[Int]] = {
    saltzDualSim(g, q, feasibleMates(g, q))
  }

  def saltzDualSim(g: Graph, q: Graph, phi: Array[Set[Int]]): Array[Set[Int]] = {
    //val phi = feasibleMates(g, q) 
    // TODO: check if any of them are empty... Although you find that out 
    // quickly enough
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
              changed = true  //TODO: possibly implicit?
            }
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
  //TODO:  Broken.
  def saltzDualSim2(g: Graph, q: Graph): Array[Set[Int]] = {
    val phi = feasibleMates(g, q) 
    var changed = true
    while (changed) {
      changed = false
      for (qNode <- 0 until q.size) {
        for (qChild <- q.adjList(qNode)) {
          var newPhi = Set[Int]()
          for (phiNode <- phi(qNode)) {
            var empty = true
            // phi(qChild).size must be less than g.adjList.size
            for (n <- phi(qChild)) {
              if(g.adjList(phiNode) contains n) {
                empty = false
                newPhi += n
              }
            }

            if (empty) { 
              phi(qNode) -= phiNode 
              changed = true  //TODO: possibly implicit?
            }
          }
          if (newPhi.isEmpty) return Array[Set[Int]]() 
          if (phi(qChild).size > newPhi.size) changed = true
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
    phi.foreach { p =>
      out.println(p)
    }
    out.close
  }
}
