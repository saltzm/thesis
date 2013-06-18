import scala.io.Source
import java.nio.file
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.FileSystems
import java.nio.file.StandardOpenOption
import java.nio.charset.Charset
import java.io.BufferedWriter

object Preprocessor extends App {

  // check command line args
  if (args.length != 6) {
    println("Args must be of the form " + 
            "<g_file_name> <g_out_file_edges> <g_out_file_label> " +
            "<q_file_name> <q_out_file_edges> <q_out_file_label>")
    System.exit(0)
  }

  val g_in_file = args(0)
  val g_out_file_edges = args(1)
  val g_out_file_cols = args(2)
  val q_in_file = args(3)
  val q_out_file_edges = args(4)
  val q_out_file_cols = args(5)

  graphToIGraph(g_in_file, g_out_file_edges, g_out_file_cols)
  graphToIGraph(q_in_file, q_out_file_edges, q_out_file_cols)

  def graphToIGraph(in_file: String, out_file_edges: String, out_file_cols: String) {
    var lines = readFileIntoLines(in_file)
    val edge_writer = createFileWriter(out_file_edges)
    val label_writer = createFileWriter(out_file_cols)
    for (line <- lines) {
      var parts = line.split("""\s+""", 3)
      var id = parts(0)
      var label = parts(1)
      if (parts.length == 3){
        var children = parts(2).split("""\s+""")
        for (child <- children if !child.trim.equals("")) 
          edge_writer.write(id + " " + child + "\n")
      }
      label_writer.write(label + "\n")
    }
    edge_writer.close
    label_writer.close
  }

  def createFileWriter(filename: String): BufferedWriter = {
    var charset = Charset.forName("US-ASCII")
    val path = FileSystems.getDefault().getPath(filename) 
    Files.newBufferedWriter(path, charset) 
  }

  def readFileIntoLines (filename: String): Iterator[String] = {
    Source.fromFile(filename).getLines
  }
}

