import scala.io.Source
object Day12 extends App {

  val testFile = "test.txt"
  val inputFile = "input.txt"

  println("Reading input")
  parse_input(testFile).map(t => solve(t._1, t._2))

  def parse_input(file_path: String) = {
    val source = Source.fromFile(file_path)
    source
      .getLines()
      .map(l => {
        val parts = l.split(" ")
        val springs = parts(0).toList
        val sequences = parts(1).split(",").toList.map(_.toInt)
        (springs, sequences)
      }).toList
  }

  def solve(parts: List[Char], sequences: List[Int]): Int = {
    println(parts)
    println(sequences)

    

    1
  }
}
