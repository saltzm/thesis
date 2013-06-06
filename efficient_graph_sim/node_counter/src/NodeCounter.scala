import scala.io.Source

object NodeCounter extends App {
  var lines = Source.stdin.getLines
  var nodes = Set[Int]()
  for (l <- lines) {
    val splitLine = l.split(" ")
    if (splitLine.size == 1){
      nodes = nodes + l.trim.toInt
    } else if (splitLine(0) contains "C") {
      println(l)
    }
  }
  println("C number of unique nodes:  " + nodes.size)
}
