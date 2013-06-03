import collection.mutable.{Queue, Set, Map, BitSet}
import collection.mutable
import java.util
import util.Collections

class Ball(parentGraph: Graph, center: Int, radius: Int) {

  var nodesInBall = mutable.Set[Int]()

  var borderNodes = mutable.Set[Int]()
  var q = mutable.Queue[(Int, Int)]()  //vertex, depth pair

  var adjSet = mutable.Map[Int, mutable.Set[Int]]()
  var parList = mutable.Map[Int, mutable.Set[Int]]()
  var ballcenter = center

  nodesInBall += center
  q.enqueue((center, 0))

  //assumes diameter always > 0
 adjSet += (center -> parentGraph.post(center) )
  parList += (center -> parentGraph.pre(center) )


  while( !q.isEmpty ){
    val (nextV, depth) = q.dequeue
    if( depth == radius ){
      borderNodes += nextV
    } else{
      val children = parentGraph.post(nextV)
      val parents = parentGraph.pre(nextV)

      for( child <- children ) {
        if(!nodesInBall.contains(child)){
          nodesInBall += child
          q.enqueue((child, depth + 1))
        }
      }

      for( parent <- parents) {
        if(!nodesInBall.contains(parent)){
          nodesInBall += parent
          q.enqueue((parent, depth + 1))
        }
      }
    }
  }

 for(n <- nodesInBall){
    val children = parentGraph.post(n)
    val parents = parentGraph.pre(n)
    adjSet.getOrElseUpdate(n, Set())
    parList.getOrElseUpdate(n, Set())

    for (child <- children if (nodesInBall contains child)){
      parList.getOrElseUpdate(child,  Set()) += n
    }
    for (parent <- parents if (nodesInBall contains parent)){
      adjSet.getOrElseUpdate(parent, Set()) += n
    }
  }
/*  for(n <- nodesInBall){*/
    //val children = parentGraph.post(n)
    //val parents = parentGraph.pre(n)
    //val bChildren = Set[Int]()
    //val bParents = Set[Int]()

    //for (child <- children if (nodesInBall contains child)){
      //bChildren += child
    //}
    //for (parent <- parents if (nodesInBall contains parent)){
      //bParents += parent
    //}
    //adjSet += (n -> bChildren) 
    //parList += (n -> bParents)
  /*}*/

  def post(i: Int): Set[Int] = {
    adjSet.getOrElse(i, Set[Int]())
  }

  def pre(i: Int): Set[Int] = {
    parList.getOrElse(i, Set[Int]())
  }

  def getBallAsString() : String = {
    var s = new StringBuilder();
    if (adjSet != null) {
      for ( i <- adjSet.keySet.toList.sorted) {
        s.append(i + "->[");
        for ( j <- adjSet.get(i).get.toList.sorted){
          s.append(j+",");
        }
        s.append("],");
      }
    }
    s.toString();
  }

  def printball(){
    println("Center: "+ballcenter)
    println("Nodes in ball: "+nodesInBall)
    for((u, children) <- adjSet){
      println(u+"\t"+children)
    }
    println("Border nodes: ")
    for(b <- borderNodes){
      println(b)
    }
    println("-----------")
  }

}
