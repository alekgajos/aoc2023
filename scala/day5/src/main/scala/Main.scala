import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.ArraySeq
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

case class Coordinate(value: Long, kind: String)

trait Converter {
  def kindIn: String
  def kindOut: String
  def apply(source: Coordinate): Coordinate
}

case class MappedConverter(
    destStart: Long,
    sourceStart: Long,
    range: Long,
    override val kindIn: String,
    override val kindOut: String
) extends Converter {
  override def apply(source: Coordinate) = {
    if (
      source.kind == kindIn && (sourceStart until sourceStart + range contains source.value)
    ) {
      Coordinate(destStart + (source.value - sourceStart), kindOut)
    } else {
      source
    }
  }
}

case class IdentityConverter(
    override val kindIn: String,
    override val kindOut: String
) extends Converter {
  override def apply(source: Coordinate) = {
    if (source.kind == kindIn) {
      Coordinate(source.value, kindOut)
    } else {
      source
    }
  }
}

object Main extends App {
  val testFile = "test.txt"
  val inputFile = "input.txt"

  val startPattern: Regex = """^seeds: ((?:\d+ )*\d+)$""".r
  val headerPattern: Regex = """^(\w+)-to-(\w+) map:$""".r
  val convertPattern: Regex = """^(\d+) (\d+) (\d+)""".r

  def process(file_path: String, useRanges: Boolean = false) = {
    val source = Source.fromFile(file_path)

    var seedNumbers: ArraySeq[Long] = Array[Long]()
    var converters: ListBuffer[Converter] = ListBuffer[Converter]()
    var convertersWithId: ListBuffer[Converter] = ListBuffer[Converter]()
    var in: String = ""
    var out: String = ""

    val conversionMap: Map[(String, String), ArraySeq[Converter]] =
      Map.empty[(String, String), ArraySeq[Converter]]

    source
      .getLines()
      .foreach(
        {
          case startPattern(seeds) => {
            seedNumbers = seeds.split(" ").map(_.toLong)
          }

          case headerPattern(entityIn, entityOut) => {
            in = entityIn
            out = entityOut
            conversionMap += ((in, out) -> ArraySeq.empty[Converter])
          }

          case convertPattern(destStart, sourceStart, range) => {
            val converter =
              new MappedConverter(
                destStart.toLong,
                sourceStart.toLong,
                range.toLong,
                in,
                out
              )
            converters += converter
          }

          case anyPattern => {}
        }
      )

    var converters_with_id: List[Converter] = converters
      .sliding(2)
      .toList
      .map { case ct =>
        if (ct(0).kindIn != ct(1).kindIn) {
          List(ct(0), IdentityConverter(ct(0).kindIn, ct(0).kindOut))
        } else {
          List(ct(0))
        }
      }
      .flatten

    converters_with_id = converters_with_id.appended(
      IdentityConverter(
        converters_with_id.last.kindIn,
        converters_with_id.last.kindOut
      )
    )

    if (useRanges) {
      val results = seedNumbers
        .grouped(2)
        .flatMap(x => x.toList(0) to x.toList(1))
        .map(seed => Coordinate(seed, "seed"))
        .map(c => converters_with_id.foldLeft(c)((c, C) => C(c)))
      println(results.map(r => r.value).min)
    } else {
      val results = seedNumbers
        .map(seed => Coordinate(seed, "seed"))
        .map(c => converters_with_id.foldLeft(c)((c, C) => C(c)))
      println(results.map(r => r.value).min)
    }
  }

  process(inputFile)
  process(inputFile, true)
}
