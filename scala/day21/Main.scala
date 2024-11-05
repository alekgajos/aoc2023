//> using scala 3.5.1
//> using option -new-syntax
//> using option -Wunused:imports

import scala.io.Source
import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

class Map2D(lines: List[String]):
  val map: Array[Array[Char]] = lines.map(line => line.toCharArray()).toArray
  lazy val width = map.head.length
  lazy val height = map.length

  def apply(p: Point): Option[Char] =
    try Some(map(p.x)(p.y))
    catch case e: ArrayIndexOutOfBoundsException => None

  def moves(p: Point): List[Point] =
    for
      x <- (-1 to 1).toList
      y <- (-1 to 1).toList
      if x * x + y * y == 1
      here = p + Point(x, y)
      if this(here).getOrElse('?') == '.' || this(here).getOrElse('?') == 'S'
    yield here

case class Point(x: Int, y: Int):
  def +(rhs: Point): Point =
    Point(this.x + rhs.x, this.y + rhs.y)


def part1(file:String, steps: Int): Int = 

  val source = Source.fromFile(file)
  val lines = source.getLines().toList

  val map = Map2D(lines)

  var start = Point(-1, -1)

  for
    y <- 0 until map.height
    x <- 0 until map.width
  do
    // print(map.map(x)(y))
    // if x == map.width - 1 then println()
    if map.map(x)(y) == 'S' then start = Point(x, y)

  println(start)

  val Q = Queue[Point]()
  Q.enqueue(start)

  val distances = HashMap[Point, Int]()
  distances.update(start, 0)

  val visited = HashSet[Point]()

  while (Q.nonEmpty) do
    val next = Q.dequeue()
    val d = distances(next)
    
    if d < steps && !visited.contains(next) then
      map
        .moves(next)
        .foreach( v =>
          distances.update(v, d+1)
          Q.enqueue(v)
        )
      visited.add(next)

  distances.filter((p,d) => d==steps).foreach(println)

  var count = distances.count((p,d) => d<=steps && d%2==0)
  count
  // distances

@main def hello(): Unit =

  println(part1("test.txt", 6))
  println(part1("input.txt", 64))