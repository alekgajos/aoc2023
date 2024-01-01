import scala.io.Source
import scala.language.implicitConversions
import scala.collection.mutable.ArrayBuffer

sealed trait ParserState
object ParserState {
  case object N extends ParserState
  case object D extends ParserState
}

case class Number(index: Int, len: Int, value: Int)

class Parser() {

  var state: ParserState = ParserState.N
  var symbols = new ArrayBuffer[Int](100)
  val gears = new ArrayBuffer[Int](100)
  var numbers = new ArrayBuffer[Number](100)
  var currentNumber = 0
  var currentIndex = -1
  var currentLen = 0

  def reset() = {
    currentNumber = 0
    currentLen = 0
    currentIndex = -1
    state = ParserState.N
  }

  def registerSymbol(i: Int, x: Char) = {
    if (x != '.') {
      symbols.append(i)
    }
    if (x == '*') {
      gears.append(i)
    }
  }

  def processChar(x: Char, i: Int) = {

    (x.isDigit, state) match {
      case (true, ParserState.N) => {
        currentNumber += x.asDigit
        currentLen += 1
        currentIndex = i
        state = ParserState.D
      }
      case (true, ParserState.D) => {
        currentNumber *= 10
        currentNumber += x.asDigit
        currentLen += 1
        state = ParserState.D
      }
      case (false, ParserState.N) => {
        registerSymbol(i, x)
        state = ParserState.N
      }
      case (false, ParserState.D) => {
        registerSymbol(i, x)
        numbers.append(new Number(currentIndex, currentLen, currentNumber))
        reset()
      }
    }
  }

  def processLine(line: String) = {

    state = ParserState.N
    numbers.clear()
    symbols.clear()
    gears.clear()

    val line_chars = line.toCharArray().zipWithIndex
    line_chars.foreach(Function.tupled(processChar))

    if (currentLen > 0) {
      numbers.append(new Number(currentIndex, currentLen, currentNumber))
      reset()
    }

    (numbers.toSeq, symbols.toSeq, gears.toSeq)
  }

}

object Main extends App {

  val test_file = "test.txt"
  val input_file = "input.txt"

  def checkAdjacentSymbols(start: Int, end: Int)(symbols: Seq[Int]): Boolean = {
    symbols.foreach(i => {
      if (i >= start - 1 && i <= end + 1) return true
    })
    return false
  }

  def findAdjacentNumbers(index: Int)(numbers: Seq[Number]): Seq[Int] = {
    numbers
      .map({
        case Number(start, len, value) => {
          if (index >= start - 1 && index <= start + len) Some(value)
          else None
        }
      })
      .flatten
  }

  def process(file_path: String): Unit = {

    val source = Source.fromFile(file_path)

    val lines_list = source.getLines().toList
    val lines_list_ext: List[String] =
      lines_list.prepended("..........").appended("..........")

    val parser = new Parser()

    val coordinates = lines_list_ext.map(parser.processLine)

    // part 1
    val part_numbers = coordinates.sliding(3).map {
      case line_list => {
        line_list(1)._1.map({
          case Number(index, len, value) => {
            val check = checkAdjacentSymbols(index, index + len - 1) _
            val ok = line_list.map(l => check(l._2)).reduce((a, b) => a || b)
            if (ok) value else 0
          }
        })
      }
    }

    println(part_numbers.flatten.reduce((a, b) => a + b))

    // day 2
    val gear_ratios = coordinates.sliding(3).map {
      // coordinates.sliding(3).foreach {
      case line_list => {
        line_list(1)._3.map({ index =>
          {
            val countFunction = findAdjacentNumbers(index) _
            val adjacent_numbers =
              line_list.map(l => countFunction(l._1)).flatten
            if (adjacent_numbers.length == 2) {
              adjacent_numbers.reduce((a, b) => a * b)
            } else {
              0
            }
          }
        })
      }
    }

    println(gear_ratios.flatten.foldLeft(0)((a, b) => a + b))

    source.close()
  }

  // process(test_file)
  process(input_file)
}
