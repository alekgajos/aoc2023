import scala.io.Source
import java.lang.Math.sqrt
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.{HashSet, HashMap}

case class Vec(x: Int, y: Int) {

  def *(other: Vec) = x * other.x + y * other.y
  def *(scale: Int) = Vec(x * scale, y * scale)

  def normalized() = {
    val magnitude = sqrt(x * x + y * y)
    Vec((x / magnitude).toInt, (y / magnitude).toInt)
  }

  def perp(): List[Vec] = (for (
    xx <- -1 to 1;
    yy <- -1 to 1;
    vv = Vec(xx, yy)
    if this * vv == 0 && vv * vv != 0
  ) yield Vec(xx, yy)).toList

}

case class Position(x: Int, y: Int) {
  def +(v: Vec) = Position(x + v.x, y + v.y)
}

case class Vertex(pos: Position, dir: Vec, distance: Int, step: Int)
    extends Ordered[Vertex] {
  def compare(other: Vertex): Int = {
    -1*this.distance.compare(other.distance)
  }
}

case class Matrix[T](M: List[List[T]]) {

  lazy val width = M.head.length
  lazy val height = M.length

  def print() = {
    M.foreach(line => {
      println(line.mkString(" "))
    })
  }

  def apply(x: Int, y: Int): Option[T] = {

    if (y >= 0 && y < M.length) {
      if (x >= 0 && x < M(y).length) {
        return Some(M(y)(x))
      }
    }
    None
  }

  def apply(pos: Position): Option[T] = apply(pos.x, pos.y)

}

class Problem(lines: List[String]) {

  val ints = lines.map(l => l.toCharArray().map(c => c.asDigit).toList)
  val M = Matrix(ints)

  def solve(part_two: Boolean): Int = {
    M.print()

    var queue = PriorityQueue[Vertex]()
    var visited = HashSet[(Position, Vec, Int)]()
    var predecessors = HashMap[Position, Position]()
    var distances = HashMap[Position, Int]()

    val start = Position(0, 0)
    List(Vec(0, 1), Vec(1, 0)).foreach(dir => {
      var path_heat = 0
      1 to 3 foreach (step => {
        val pos = start + dir * step
        predecessors.addOne((pos, start+dir*(step-1)))
        path_heat += M(pos).get
        distances.addOne((pos, path_heat))
        queue.enqueue(Vertex(pos, dir, path_heat, step))
      })

    })

    while (!queue.isEmpty) {

      val vertex = queue.dequeue()

      if (!visited.contains((vertex.pos, vertex.dir, vertex.step))) {

        visited.add((vertex.pos, vertex.dir, vertex.step))

        // if (vertex.pos == Position(M.width - 1, M.height - 1)) {
        //   return vertex.distance
        // }

        vertex.dir
          .perp()
          .foreach(dir => {

            var path_heat = 0
            var prev_vtx = vertex.pos

            1 to 3 foreach (step => {

              val pos = vertex.pos + dir * step
              if(!visited.contains((pos, dir, step))){
              M(pos) match {
                case Some(heat) => {
                  path_heat += heat
                  val old_distance = distances.get(pos).getOrElse(1000)
                  if (vertex.distance + path_heat <= old_distance) {
                    distances.addOne((pos, vertex.distance + path_heat))
                    queue.enqueue(
                      Vertex(pos, dir, vertex.distance + path_heat, step)
                    )
                    predecessors.addOne((pos, prev_vtx))
                  }
                }
                case None =>
              }}
              prev_vtx = pos
            })
          })

      }
    }


    var used = HashSet[Position]()
    var pos = Position(M.width-1, M.height-1)

    while (pos != start) {
      pos = predecessors.get(pos).get
      used.addOne(pos)
    }

    0 until M.height foreach(yy => {
      0 until M.width foreach {
        xx => {
          val pos = Position(xx, yy)
          if (used.contains(pos)){
            print(s"|${M(pos).get}| ")
          }else{
            print(s" ${M(pos).get}  ")
          }
        }
      }
      println()
    })

    distances.get(Position(M.width-1, M.height-1)).get
  }

}

object Day13 extends App {

  val testFile = "test.txt"
  val inputFile = "input.txt"

  def process(filePath: String, part_two: Boolean) = {
    val lines = Source.fromFile(filePath).getLines().toList
    val heat_loss = Problem(lines).solve(part_two)

    println(heat_loss)
  }

  process(testFile, false)
  // process(inputFile, 0)
  // process(testFile, 1)
  // process(inputFile, 1)
}
