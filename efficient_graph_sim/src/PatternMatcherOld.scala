import java.io._
import scala.annotation._

object PatternMatcherOld {

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
        val phiCopy = phi.map(x=>x)
        phiCopy(depth) = Set[Int](i)
        matches = matches ++ saltzIso(g, q, saltzDualSim(g, q, phiCopy), depth + 1)
      }
      matches
    }
  }

  def saltzIso2(g: Graph, q: Graph): Set[Array[Set[Int]]] = {
    var matches = Set[Array[Set[Int]]]()
    var initPhi = saltzDualSim(g, q, feasibleMates(g, q))
    saltzIso_(g, q, initPhi, 0)
  
    def saltzIso_(g: Graph, q: Graph, phi: Array[Set[Int]], depth: Int) {
      //TODO:  check if phi is already an isomorphism?
      if (depth == q.size) matches += phi 
      else if (!phi.isEmpty) {
        for (i <- phi(depth) if (!phi.slice(0, depth).flatten.contains(i))) {
          val phiCopy = phi.map(x=>x)
          phiCopy(depth) = Set[Int](i)
          saltzIso_(g, q, saltzDualSim(g, q, phiCopy), depth + 1)
        }
      }
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
  
  //def saltzIsoTail(g: Graph, q: Graph): Set[Array[Set[Int]]] = {
    //var matches = Set[Array[Set[Int]]]()
    //var initPhi = saltzDualSim(g, q, feasibleMates(g, q))
    //saltzIsoTail_(g, q, initPhi, Array.ofDim[Int](q.size), 0) 

    //def saltzIsoTail_(g: Graph, q: Graph, phi: Array[Set[Int]], 
                      //curPos: Array[Int], maxPos: Array[Int], depth: Int) {
      //if (depth == q.size) {
        //matches += phi
        //if (currentPos(depth) == phi(depth.size)
      //}
      //else if (!phi.isEmpty) {
        //if (!contains(phi, i, depth)) {
          //val phiCopy = phi.map(x=>x)
          //phiCopy(newDepth) = Set[Int](i)
          //var newDepth = depth 
          //var newI = i + 1
          //if(newI == phi(depth.size)) {
            //newI = 0
            //newDepth = depth + 1
          //}
          //saltzIso_(g, q, saltzDualSim(g, q, phiCopy), newI, newDepth)
        //}
      //}
    //}

    //def contains(phi: Array[Set[Int]], ele: Int, depth: Int): Boolean = {
      //for (i <- 0 until depth) if (phi(i) contains ele) return true
      //false 
    //}
    //matches 

  /*}*/
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

  def saltzIsoPrint(g: Graph, q: Graph, file: String): Int = {
    val out = new PrintWriter(file)
    var nRes = 0
    var initPhi = saltzDualSim(g, q, feasibleMates(g, q))
    saltzIso_(g, q, initPhi, 0) 
  
    def saltzIso_(g: Graph, q: Graph, phi: Array[Set[Int]], depth: Int) {
      if (depth == q.size) {
        out.println("Res " + nRes + ": ")
        phi.foreach (out.println(_))
      }
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
    out.close
    nRes
  }

  def saltzDualSim(g: Graph, q: Graph): Array[Set[Int]] = 
    saltzDualSim(g, q, feasibleMates(g, q))

  // TODO: add depth component for isomorphism so you don't check unneccessary
  //      shit
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
              if (phi(qNode).isEmpty) return Array[Set[Int]]()
              changed = true  //TODO: possibly implicit?
            }
            //TODO: would mutable make it faster?
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
