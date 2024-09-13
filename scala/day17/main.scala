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

case class Vertex(pos: Position, dir: Vec, step: Int)

case class Candidate(vertex: Vertex, distance: Int) extends Ordered[Candidate] {
  def compare(other: Candidate): Int = {
    -1 * this.distance.compare(other.distance)
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

  def solve(min_steps: Int, max_steps: Int): Int = {

    var queue = PriorityQueue[Candidate]()
    var visited = HashSet[Vertex]()
    var predecessors = HashMap[Vertex, Vertex]()
    var distances = HashMap[Vertex, Int]()

    val start = Position(0, 0)
    var prev_vtx = Vertex(start, Vec(0, 0), 0)
    List(Vec(0, 1), Vec(1, 0)).foreach(dir => {
      var path_heat = 0
      1 to max_steps foreach (step => {
        val pos = start + dir * step
        val vertex = Vertex(pos, dir, step)
        predecessors.addOne((vertex, prev_vtx))
        prev_vtx = vertex
        val heat = M(pos)
        if (!heat.isEmpty) {

          path_heat += M(pos).get
          distances.addOne((vertex, path_heat))

          if(step >= min_steps){
            queue.enqueue(Candidate(vertex, path_heat))
          }
        }
      })

    })


    while (!queue.isEmpty) {

      val Candidate(vertex, distance) = queue.dequeue()


      if (!visited.contains(vertex)) {

        visited.add(vertex)

        // if (vertex.pos == Position(M.width - 1, M.height - 1)) {
        //   return vertex.distance
        // }

        vertex.dir
          .perp()
          .foreach(dir => {

            var path_heat = 0
            var prev_vtx = vertex

            1 to max_steps foreach (step => {

              val pos = vertex.pos + dir * step
              val this_vertex = Vertex(pos, dir, step)

              // if(!visited.contains((pos, dir, step))){

              M(pos) match {
                case Some(heat) => {

                  path_heat += heat
                  val old_distance = distances.get(this_vertex).getOrElse(Int.MaxValue)

                  if (distance + path_heat <= old_distance) {
                    distances.addOne((this_vertex, distance + path_heat))
                    predecessors.addOne((this_vertex, prev_vtx))

                    if(step >= min_steps){
                      queue.enqueue(
                        Candidate(Vertex(pos, dir, step), distance + path_heat)
                      )
                    }

                  }

                  prev_vtx = vertex

                }
                case None =>
              }
              prev_vtx = this_vertex
            })
          })

      }
    }

    var used = HashSet[Position]()
    var pos = Position(M.width - 1, M.height - 1)

    val dists = for (
      (vertex, distance) <- distances
      if vertex.pos == Position(M.width - 1, M.height - 1) && vertex.step >= min_steps
    ) yield { distance }
    val min_dist = dists.min()
    val final_vertex = distances
      .find((v, d) =>
        v.pos == Position(M.width - 1, M.height - 1) && d == min_dist
      )
      .get(0)

    var vtx = final_vertex

    while (vtx.pos != start) {
      vtx = predecessors.get(vtx).get
      used.addOne(vtx.pos)
    }

    0 until M.height foreach (yy => {
      0 until M.width foreach { xx =>
        {
          val pos = Position(xx, yy)
          if (used.contains(pos)) {
            print(s"|${M(pos).get}| ")
          } else {
            print(s" ${M(pos).get}  ")
          }
        }
      }
      println()
    })

    min_dist
  }
}

object Day13 extends App {

  val testFile = "test.txt"
  val testFile2 = "test2.txt"
  val inputFile = "input.txt"

  def process(filePath: String, part_two: Boolean) = {
    val lines = Source.fromFile(filePath).getLines().toList
    val heat_loss = if (part_two) Problem(lines).solve(4, 10) else Problem(lines).solve(1,3)

    println(heat_loss)
  }

  // process(testFile, false)
  // process(inputFile, false) // 1155
  process(testFile, true)
  process(testFile2, true)
  process(inputFile, true)
}
