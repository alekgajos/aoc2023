//> using scala 3.4.1

import scala.io.Source
import scala.collection.mutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Buffer
import scala.collection.mutable.Stack

sealed trait Axis

case object XAxis extends Axis
case object YAxis extends Axis

object Matrix {

  def apply(M: ListBuffer[String]) = {
    new Matrix(M.mkString("").toCharArray().toBuffer, M.length, M.head.length)
  }

}

class Matrix(var data: Buffer[Char], nrows: Int, ncols: Int) {

  def getSeq(index: Int, axis: Axis) = {
    axis match
      case XAxis =>
        data.slice(index * size(YAxis), until = (index + 1) * size(YAxis))
      case YAxis => data.drop(index).grouped(size(YAxis)).map(_.head).toList
  }

  def size(axis: Axis) = {
    axis match
      case XAxis => ncols
      case YAxis => nrows
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

  def solveAll() = parse().map(solve(_))

  def solve(M: Matrix) = {

    val columnWise: Axis = YAxis
    var result: Int = 0
    var rollingStones: Int = 0
    var stoneStack: Stack[Int] = Stack()

    for (index <- (0 until M.size(columnWise))) {

      rollingStones = 0
      val col = M.getSeq(index, columnWise).reverse.zipWithIndex
      for ((c, i) <- col) {
        c match {
          case 'O' => {
            rollingStones += 1
          }
          case '#' => {
            for (k <- 1 to rollingStones) {
              result += i - k + 1
            }
            rollingStones = 0
          }
          case '.' => {}
        }
      }

      for (k <- 1 to rollingStones) {
        val rowWise: Axis = XAxis
        result += M.size(rowWise) - k + 1
      }
    }

    // M.M.foreach(println(_))
    // println("=========")

    result
  }

}

object Day14 extends App {

  val testFile = "test.txt"
  val inputFile = "input.txt"

  def process(filePath: String) = {

    println(Problem(filePath).solveAll().sum)
  }

  process(testFile)
  process(inputFile)
  // process(testFile, 1)
  // process(inputFile, 1)
}
