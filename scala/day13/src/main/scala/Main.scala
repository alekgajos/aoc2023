import scala.io.Source
import scala.collection.mutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait Axis

final case class XAxis() extends Axis
final case class YAxis() extends Axis

case class Matrix(M: ListBuffer[String]) {

  def getSeq(index: Int)(using axis: Axis) = {
    axis match
    case XAxis() => M.apply(index).map(c => if(c=='#')1 else 0).toList
    case YAxis() => M.map(line => line.toList.apply(index)).map(c => if(c=='#')1 else 0).toList
    // case YAxis() => M.map(line => line.chars().toArray().apply(index)).map(c=> if())toList

  }

  def size()(using axis: Axis) = {
    axis match
      case XAxis() => M.length
      case YAxis() => M.head.length
  }

  def checksums()(using  axis: Axis) = {
    (0 to size()).map(i => getSeq(i).sum)
  }

  // lazy val row_checksums = M.map(_.chars().sum()).toList
  // lazy val col_checksums =
  //   (0 to M.length).map(getColumn(_)).map(l => l.sum).toList

  // def checkMatch(index: Int, span: Int, rows: Boolean) = {
  //   if (rows) {
  //     M(index) == M(index + span)
  //   } else {
  //     getColumn(index) == getColumn(index + span)
  //   }
  // }

}

class Problem(lines: List[String]) {

  var matrices = ListBuffer[Matrix]()
  matrices.prepend(Matrix(ListBuffer()))

  def findMatrices(l: List[String]): Unit = {

    val head = l.headOption

    head match {
      case Some(line) => {
        line.length match {
          case 0 => {
            matrices.prepend(Matrix(ListBuffer()))
          }
          case _ => {
            matrices.head.M.append(line)
          }
        }
        findMatrices(l.tail)
      }
      case None =>
    }

  }

  def solveAll() = {
    matrices.map(solve(_))
  }

  def solve(M: Matrix) = {

  {
    given Axis = XAxis()
    println(M.size())
  }
  {
    given Axis = YAxis()
    println(M.size())
    // println(M.checksums())
  }
    // println(M.col_checksums)

    // val row_matches = M.row_checksums
    //   .sliding(2)
    //   .map(l => l(0) == l(1))
    //   .zipWithIndex
    //   .filter((ok, i) => ok)
    //   .map((ok, i) => i)
      // .filter(M.checkMatch(_, 1, true))

    // println(row_matches.toList)

    // row_matches.map

    1
  }

}

object Day13 extends App {

  val testFile = "test.txt"
  val inputFile = "input.txt"

  def parseInput(filePath: String) = {
    val lines = Source.fromFile(filePath).getLines().toList
    val problem = Problem(lines)
    problem.findMatrices(lines)

    problem.matrices.foreach(m => {
      m.M.foreach(l => println(l))
      println()
    })

    println(problem.solveAll())
  }

  parseInput(testFile)
  // parseInput(inputFile)

  println("Hello, World!")
}
