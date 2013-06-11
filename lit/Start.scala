      


case class Edge(from: Int, to: Int, edge_label: LabelEdge)
case class LabelEdge(from_label: Int, to_label: Int)
object Test extends App {

  //Read graph file into Edges
  //Read query file into Edges
  //Remove all edges from graph that aren't in query edges
  
  var edges = Set[Edge]()
  var lines = Source.fromFile("g.txt").getLines
  for (line <- lines) {
    var parts = line.split("""\s+""", 3)
    var id = parts(0)
    var label = parts(1)
    if (parts.length == 3){
      var children = parts(2).split("""\s+""")
      for (child <- children if !child.trim.equals("")) 
        (id + " " + child + "\n")
    }
    label_writer.write(label + "\n")
  }
  edge_writer.close
  label_writer.close

  


}
