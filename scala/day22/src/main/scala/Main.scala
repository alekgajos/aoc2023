import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

case class Coords(x: Int, y: Int, z: Int)

case class Brick(s: Coords, e: Coords):
  lazy val xy: Seq[Coords] = 
    for
      sx <- s.x to e.x
      sy <- s.y to e.y
      dz = scala.math.abs(s.z - e.z) + 1
    yield
      Coords(sx, sy, dz)

  lazy val lowz: Int = s.z.min(e.z)

object Coords:
  def apply(s: Seq[Int]) = 
    new Coords(s(0), s(1), s(2))

class Surface(data: ArrayBuffer[Int], bricks: ArrayBuffer[Int], width: Int, height: Int):

  val supporters = HashSet[Int]()
  val all_bricks = HashSet[Int]()
 
  private def index(x: Int, y: Int): Int = y*width + x 

  def get(x: Int, y: Int): (Int, Int) = 
    (data(index(x,y)), bricks(index(x,y)))

  def get(c: Coords): (Int, Int) = get(c.x, c.y)

  def set(x: Int, y: Int, z: Int): Unit = 
    data(index(x,y)) = z

  def set_index(x: Int, y: Int, idx: Int) : Unit = 
    bricks(index(x,y)) = idx

  def add(b: Brick, idx: Int): Unit = 
    
    all_bricks += idx

    val heights = b.xy.map(coords => {
      this.get(coords)._1
    }
    )

    // indices of all bricks directly supporting this one
    val covered = b.xy.map(this.get(_)).filter(_._1 == heights.max).map(_._2)
    
    if (covered.distinct.length == 1) then // exactly one brick supports this one
      println(s"brick $idx is supported by $covered")
      supporters ++= covered
    
    val new_height = heights.max() + b.xy.head.z


    b.xy.map(coords=> set(coords.x, coords.y, new_height))
    b.xy.map(coords=> set_index(coords.x, coords.y, idx))

  def get_top(): Int = 
    (all_bricks &~ supporters).size

object Day22:
  def parseInput(file: String) = 

    val source = Source.fromFile(file)
    val lines = source.getLines().toList
    
    lines.map(line => line.split("~").map(str => Coords(str.split(",").map(_.toInt))
    )).map(arr => Brick(arr(0), arr(1)))

@main def hello(): Unit =

  val input = Day22.parseInput("test.txt")
  // val input = Day22.parseInput("input.txt")
  
  val xys = input.map(_.xy)
  val width = input.map(b => Seq(b.s, b.e)).flatten.map(_.x).max + 1
  val height = input.map(b => Seq(b.s, b.e)).flatten.map(_.y).max + 1

  println(s"width=$width, height=$height")
  
  val surface = Surface(ArrayBuffer.fill(width*height+1)(0), ArrayBuffer.fill(width*height+1)(-1), width, height)
  // println(input.sortBy(_.lowz).take(100))
  input.sortBy(_.lowz).zipWithIndex.foreach((brick, idx) => surface.add(brick, idx))

  println(surface.get_top())