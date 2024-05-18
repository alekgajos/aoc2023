import scala.io.Source
import scala.collection.mutable.ArraySeq
import scala.collection.mutable

def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() { self =>
  override def apply(key: I) = self.synchronized(getOrElseUpdate(key, f(key)))
}

class Problem(parts: List[Char], sequences: List[Int]) {

  def printMatrix(): Unit = {

    println(parts)
    println(sequences)

    List
      .range(0, parts.length)
      .foreach(i => {
        print(s"$i:\t")
        List
          .range(1, sequences.length + 1)
          .foreach(j => {
            val x = countSolutions(i, j)
            print(s"$x\t| ")
          })
        print("\n")
      })
  }

  def positionAllowed(lastIndex: Int, subSequences: Int): Boolean = {
    val seqLen = sequences(subSequences - 1)

    val edges =
      Seq(parts.lift(lastIndex - seqLen), parts.lift(lastIndex + 1)).flatten

    if (edges.contains('#')) {
      false // sequence would be longer than required
    } else if (
      subSequences == sequences.length
      && parts
        .slice(lastIndex + 1, parts.length)
        .contains('#')
    ) {
      false // last sequence and #-s left => impossible placement
    } else if (
      subSequences == 1
      && parts.slice(0, lastIndex - seqLen).contains('#')
    ) {
      false // # left before the 1st sequence
    } else if (
      parts
        .slice(lastIndex - seqLen + 1, lastIndex + 1)
        .count(c => c == '#' || c == '?') == seqLen
    ) {
      true
    } else {
      false
    }
  }

  def minIndex(subSequences: Int): Int =
    sequences
      .take(subSequences)
      .sum + subSequences - 1 - 1 // last -1 is for 0-based indexing

  def countAllSolutions(): Long =
    countSolutions(parts.length - 1, sequences.length)

  lazy val countSolutions: ((Int, Int)) => Long = memoize {

    case (_, 0)          => 1 // no sequences to fit
    case (i, _) if i < 0 => 0 // subarray of springs smaller than 1
    case (lastIndex, subSequences) => {

      if (lastIndex < minIndex(subSequences)) {
        0 // not enough space for 1..subSequences sequences
      } else {

        val seqLen = sequences(subSequences - 1)

        // number of possible placements of 1..subSequences ending at lastIndex
        val next = if (positionAllowed(lastIndex, subSequences)) {
          countSolutions(
            lastIndex - seqLen - 1,
            subSequences - 1
          )
        } else {
          0
        }

        // number of possible placements of 1..subSequences ending before lastIndex
        val prev = countSolutions(lastIndex - 1, subSequences)

        if (parts(lastIndex) == '#') {
          // only placing subSequences in the smaller subaray is not valid
          // as it would leave out unused '#' to the right
          next
        } else {
          next + prev
        }
      }
    }
  }

}

object Day12 extends App {

  def unfold[T](l: List[T], separator: T) = List.fill(5)(l).reduce((a,b) => a:::List(separator):::b)
  def unfold[T](l: List[T]) = List.fill(5)(l).reduce((a,b) => a:::b)

  def parse_input(file_path: String, partTwo: Boolean = false) = {
    val source = Source.fromFile(file_path)
    source
      .getLines()
      .map(l => {
        val parts = l.split(" ")
        val springs = parts(0).toList
        val sequences = parts(1).split(",").toList.map(_.toInt)
        if (partTwo) {
          (unfold(springs, '?'), unfold(sequences))
        } else {
          (springs, sequences)
        }
      })
      .toList
  }

  def solve(parts: List[Char], sequences: List[Int]): Long = {
    // Problem(parts, sequences).printMatrix()
    Problem(parts, sequences).countAllSolutions()
  }
  val testFile = "test.txt"
  val inputFile = "input.txt"

  val solutions = parse_input(inputFile)
    .map(t => solve(t._1, t._2))

  println(solutions.sum)

  val solutions2 = parse_input(inputFile, true)
    .map(t => solve(t._1, t._2))

  println(solutions2.sum)
}
