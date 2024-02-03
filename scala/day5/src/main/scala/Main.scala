import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.ArraySeq
import scala.collection.mutable.Map

case class Converter(destStart: Int, sourceStart: Int, range: Int) {
  def apply(source: Int) = {
    if (source < sourceStart + range && source >= sourceStart) {
      Some(destStart + (source - sourceStart))
    }else{
      None
    }
  }
}

object Main extends App {
  val testFile = "test.txt"
  val inputFile = "input.txt"

  val startPattern: Regex = """^seeds: ((?:\d+ )*\d+)$""".r
  val headerPattern: Regex = """^(\w+)-to-(\w+) map:$""".r
  val convertPattern: Regex = """^(\d+) (\d+) (\d+)""".r

  def process(file_path: String) = {
    val source = Source.fromFile(file_path)

    var seedNumbers: ArraySeq[Int] = Array[Int]()
    var in: String = ""
    var out: String = ""
  
    val conversionMap: Map[(String, String), ArraySeq[Converter]] = Map.empty[(String, String), ArraySeq[Converter]]

    source
      .getLines()
      .foreach(
        {
          case startPattern(seeds) => {
            seedNumbers = seeds.split(" ").map(_.toInt)
          }

          case headerPattern(entityIn, entityOut) => {
            in = entityIn
            out = entityOut
            conversionMap += ((in, out) -> ArraySeq.empty[Converter])
          }

          case convertPattern(destStart, sourceStart, range) => {
            val converter = new Converter(destStart.toInt, sourceStart.toInt, range.toInt)
            conversionMap.get((in, out)).flatten :+ converter
          }

          case _ => {
            println("did not match: ")
          }
        }
      )
  }

  process(testFile)
}
