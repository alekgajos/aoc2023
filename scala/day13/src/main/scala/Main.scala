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
      case XAxis() => M.apply(index).map(c => if (c == '#') 1 else 0).toList
      case YAxis() =>
        M.map(line => line.toList.apply(index))
          .map(c => if (c == '#') 1 else 0)
          .toList
  }

  def size()(using axis: Axis) = {
    axis match
      case XAxis() => M.length
      case YAxis() => M.head.length
  }

  def checksums()(using axis: Axis) = {
    (0 until size()).map(i => getSeq(i).sum)
  }

  def checkMatch(index: Int, span: Int)(using axis: Axis) = {
    getSeq(index - span)
      .zip(getSeq(index + 1 + span))
      .map(_ == _)
      .count(_ == false)
  }

  def matches(diffThreshold: Int)(using axis: Axis) = checksums()
    .sliding(2)
    .map(l => (l(0) - l(1)).abs <= diffThreshold)
    .zipWithIndex
    .filter((ok, i) => ok)
    .map((ok, i) => i)
    .filter(checkMatch(_, 0) <= diffThreshold)
    .toList

  def checkReflection(index: Int)(using axis: Axis) = {

    val lowerRange = index
    val upperRange = size() - index - 2
    val range = lowerRange.min(upperRange)

    if ((0 to range).isEmpty) {
      true
    } else {
      (0 to range).map(checkMatch(index, _)).sum
    }
  }

}

class Problem(lines: List[String], diffThreshold: Int) {

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

  def solveAll() = matrices.map(solve(_))

  def solve(M: Matrix) = {

    val xReflections = {
      given Axis = XAxis()
      M.matches(diffThreshold)
        .filter(M.checkReflection(_) == diffThreshold)
        .map(_ + 1)
    }.headOption.getOrElse(0)

    val yReflections = {
      given Axis = YAxis()
      M.matches(diffThreshold)
        .filter(M.checkReflection(_) == diffThreshold)
        .map(_ + 1)
    }.headOption.getOrElse(0)

    100 * xReflections + yReflections
  }

}

object Day13 extends App {

  val testFile = "test.txt"
  val inputFile = "input.txt"

  def process(filePath: String, diffThreshold: Int) = {
    val lines = Source.fromFile(filePath).getLines().toList
    val problem = Problem(lines, diffThreshold)
    problem.findMatrices(lines)
    problem.matrices = problem.matrices.reverse

    println(problem.solveAll().sum)
  }

  process(testFile, 0)
  process(inputFile, 0)
  process(testFile, 1)
  process(inputFile, 1)
}
