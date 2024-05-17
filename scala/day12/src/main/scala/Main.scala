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

  def countAllSolutions(): Int =
    countSolutions(parts.length - 1, sequences.length)

  lazy val countSolutions: ((Int, Int)) => Int = memoize {

    case (_, 0)          => 1 // no sequences to fit
    case (i, _) if i < 0 => 0 // subarray of springs smaller than 1
    case (lastIndex, subSequences) => {

      val minIndex =
        sequences
          .take(subSequences)
          .sum + subSequences - 1 - 1 // last -1 is for 0-based indexing
      val seqLen = sequences(subSequences - 1)

      if (lastIndex < minIndex) {
        0 // not enough space for 0..j sequences
      } else {
        var lastPositionPossible: Int =
          if (lastIndex - seqLen >= 0 && parts(lastIndex - seqLen) == '#') {
            0 // sequence would be longer than required
          } else if (
            lastIndex + 1 < parts.length && parts(lastIndex + 1) == '#'
          ) {
            0 // sequence would be longer than required
          } else {
            val potentiallyBroken: Int = parts
              .slice(lastIndex - seqLen + 1, lastIndex + 1)
              .count(c => c == '#' || c == '?')
            if (potentiallyBroken == seqLen) {
              1
            } else {
              0
            }
          }

        // last sequence and #-s left => impossible placement
        if (
          subSequences == sequences.length
          && parts
            .slice(lastIndex + 1, parts.length)
            .count(_ == '#') > 0
        ) {
          lastPositionPossible = 0
        }
        if (
          subSequences == 1
          && parts.slice(0, lastIndex - seqLen).contains('#')
        ) {
          lastPositionPossible = 0 // # left before the 1st sequence
        }

        val next = countSolutions(
          lastIndex - seqLen - 1,
          subSequences - 1
        )
        val prev = countSolutions(lastIndex - 1, subSequences)

        if (lastPositionPossible == 1) {
          if (parts(lastIndex) == '#') {
            next
          } else {
            next + prev
          }
        } else {
          if (parts(lastIndex) == '#') {
            0
          } else {
            prev
          }
        }

        // println(s"next = $next, prev=$prev, possible=$lastPositionPossible")

      }
      // println(s"minIndex = $minIndex")

    }

  }

}

object Day12 extends App {

  val testFile = "test.txt"
  val inputFile = "input.txt"

  // val solutions = parse_input(testFile)
  val solutions = parse_input(inputFile)
    // .reverse
    // .take(34)
    .map(t => solve(t._1, t._2))

  println(solutions.mkString("\n"))

  // println(solutions)
  println(solutions.sum)

  def parse_input(file_path: String) = {
    val source = Source.fromFile(file_path)
    source
      .getLines()
      .map(l => {
        val parts = l.split(" ")
        val springs = parts(0).toList
        val sequences = parts(1).split(",").toList.map(_.toInt)
        (springs, sequences)
      })
      .toList
  }

  def solve(parts: List[Char], sequences: List[Int]): Int = {

    // println(parts)
    // println(sequences)
    Problem(parts, sequences).printMatrix()
    Problem(parts, sequences).countAllSolutions()

  }
}
