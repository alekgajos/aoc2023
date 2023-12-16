import scala.io.Source
import scala.language.implicitConversions

object Main extends App {

  val test_file = "test.txt"
  val input_file = "input.txt"

  var max_r: Int = 0
  var max_g: Int = 0
  var max_b: Int = 0

  implicit def bool2int(b: Boolean) = if (b) 1 else 0

  def setMax(color: String, value: Int) = {
    color.trim match {
      case "red"   => if (value > max_r) max_r = value
      case "green" => if (value > max_g) max_g = value
      case "blue"  => if (value > max_b) max_b = value
    }
  }

  def resetMax() = {
    max_r = 0
    max_g = 0
    max_b = 0
  }

  def processColor(draw: String): Int = {
    val Array(count_str, color) = draw.trim.split(" ")
    val count = count_str.toInt

    setMax(color, count)

    val res: Int = color.trim match {
      case "red"   => count <= 12
      case "green" => count <= 13
      case "blue"  => count <= 14
    }
    res
  }

  def processDraw(game: String): Int = {
    val ok = game
      .split(", ")
      .map(processColor)
      .reduce((a, b) => a * b)
    ok
  }

  def processGame(line: String): (Int, Int) = {

    resetMax()

    val Array(game_string, rest) = line.split(":")
    val number = game_string.split(" ")(1).trim.toInt
    val ok = rest
      .split(";")
      .map(processDraw)
      .reduce((a, b) => a * b)
    val result1 = if (ok == 1) number else 0

    val result2 = max_r * max_g * max_b;
    (result1, result2)
  }

  def process(file_path: String): Unit = {

    val source = Source.fromFile(file_path)

    val result = source
      .getLines()
      .map(processGame)
      .reduce((a, b) => (a._1 + b._1, a._2 + b._2))

    println(result)

    source.close()
  }

  process(test_file)
  process(input_file)
}
