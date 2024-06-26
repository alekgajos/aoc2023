//> using scala 3.4.1

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import scala.io.Source

sealed trait Axis
case object XAxis extends Axis
case object YAxis extends Axis

sealed trait Direction
case object Forward extends Direction
case object Backward extends Direction

case class NESW(axis: Axis, dir: Direction)

val North = NESW(YAxis, Backward)
val South = NESW(YAxis, Forward)
val East = NESW(XAxis, Forward)
val West = NESW(XAxis, Backward)

object Matrix {
  def apply(M: ListBuffer[String]) = {
    new Matrix(M.mkString("").toCharArray().toBuffer, M.length, M.head.length)
  }
}

class Matrix(var data: Buffer[Char], nrows: Int, ncols: Int) {

  def copy() = {
    new Matrix(data.clone(), nrows, ncols)
  }

  def otherAxis(axis: Axis) = axis match
    case XAxis => YAxis
    case YAxis => XAxis

  def getSeq(index: Int, vector: NESW) = {

    val seq = vector.axis match
      case XAxis =>
        data.slice(index * size(YAxis), until = (index + 1) * size(YAxis))
      case YAxis => data.drop(index).grouped(size(YAxis)).map(_.head).toList

    vector.dir match
      case Forward  => seq
      case Backward => seq.reverse
  }

  def setChar(c: Char, long: Int, perp: Int, vector: NESW) = {

    val longPos = vector.dir match
      case Forward  => long
      case Backward => size(vector.axis) - long - 1

    val index = vector.axis match
      case XAxis => perp * ncols + longPos
      case YAxis => longPos * nrows + perp
    data.update(index, c)
  }

  def size(axis: Axis) = {
    axis match
      case XAxis => ncols
      case YAxis => nrows
  }

  def print() = {
    println()
    for (i <- 0 until nrows) {
      println(getSeq(i, East).mkString(""))
    }
  }
}

class Problem(filePath: String) {

  var prematrices = ListBuffer[ListBuffer[String]]()
  prematrices.prepend(ListBuffer())

  def findMatrices(l: List[String]): Unit = {

    val head = l.headOption

    head match {
      case Some(line) => {
        line.length match {
          case 0 => {
            prematrices.prepend(ListBuffer())
          }
          case _ => {
            prematrices.head.append(line)
          }
        }
        findMatrices(l.tail)
      }
      case None =>
    }
  }

  def parse() = {
    val lines = Source.fromFile(filePath).getLines().toList
    findMatrices(lines)
    prematrices.reverse.map(Matrix(_))
  }

  def solvePart1() = parse().map(m => calcLoadNorth(tilt(m, North))).sum

  var fastForwarded: Boolean = false
  val nrounds: Long = 1000000000

  @tailrec
  final def cycleTiltsNtimes(M: Matrix, times: Long): Matrix = {

    if (times == 0) {
      return M
    }

    cycleTilts(M) match {
      case (newM, iteration) if iteration > 0 => {
        var newTimes = times
        if (!fastForwarded) {
          fastForwarded = true
          val n = nrounds - times
          newTimes = (nrounds - iteration) % (n - iteration)
          if (newTimes == 0) {
            return M
          }
        }
        cycleTiltsNtimes(newM, newTimes - 1)
      }
      case (newM, 0) => {
        cycleTiltsNtimes(newM, times - 1)
      }
    }
  }

  def solvePart2() = {
    var M = parse().head
    M = cycleTiltsNtimes(M, nrounds)
    calcLoadNorth(M)
  }

  def tilt(origM: Matrix, vector: NESW): Matrix = {

    val M: Matrix = origM.copy()

    var rollingStones: Int = 0
    var stoneStack: Stack[Int] = Stack()

    for (index <- (0 until M.size(vector.axis))) {

      rollingStones = 0
      val col = M.getSeq(index, vector).zipWithIndex
      for ((c, i) <- col) {
        c match {
          case 'O' => {
            rollingStones += 1
            stoneStack.push(i)
          }
          case '#' => {
            var newPos = i - 1
            while (!stoneStack.isEmpty) {
              val oldPos = stoneStack.pop()
              M.setChar('.', oldPos, index, vector)
              M.setChar('O', newPos, index, vector)
              newPos -= 1
            }
            rollingStones = 0
            stoneStack.clear()
          }
          case '.' => {}
        }
      }

      var newPos = M.size(M.otherAxis(vector.axis)) - 1
      while (!stoneStack.isEmpty) {
        val oldPos = stoneStack.pop()
        M.setChar('.', oldPos, index, vector)
        M.setChar('O', newPos, index, vector)
        newPos -= 1
      }
    }

    M
  }

  def calcLoadNorth(M: Matrix) = {
    var result: Int = 0
    val vector = North

    for (index <- (0 until M.size(vector.axis))) {
      val col = M.getSeq(index, vector).zipWithIndex
      for ((c, i) <- col) {
        c match {
          case 'O' => {
            result += i + 1
          }
          case _ => {}
        }
      }
    }
    result
  }

  val cache = new mutable.HashMap[String, Long]()
  var nTilts: Long = 0

  def cycleTilts(M: Matrix): (Matrix, Long) = {

    nTilts += 1
    val key = M.data.mkString
    val tilts = Seq(North, West, South, East)
    val newMatrix = tilts.foldLeft(M)(tilt(_, _))

    cache.get(key) match
      case None => {
        cache.addOne(key -> (nTilts - 1))
        (newMatrix, 0)
      }
      case Some(iteration) => {
        (newMatrix, iteration)
      }

  }

}

object Day14 extends App {

  val testFile = "test.txt"
  val inputFile = "input.txt"

  def process(filePath: String) = {
    println(Problem(filePath).solvePart1())
    println(Problem(filePath).solvePart2()) 
  }

  process(testFile)
  process(inputFile)
}
