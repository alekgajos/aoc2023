import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.ArraySeq
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

case class Range(start: Long, length: Long, kind: String) {
  def end() = start + length - 1
}

trait Converter {
  def kindIn: String
  def kindOut: String
  def apply(source: Range): Seq[Range]
}

case class MappedConverter(
    destStart: Long,
    sourceStart: Long,
    range: Long,
    override val kindIn: String,
    override val kindOut: String
) extends Converter {

  def sourceEnd() = sourceStart + range - 1

  override def apply(source: Range) = {
    if (source.kind == kindIn) {
      val overlapStart = source.start.max(sourceStart)
      val overlapEnd = source.end.min(sourceEnd)
      if (overlapEnd - overlapStart >= 0) {
        List(
          Range(source.start, overlapStart - source.start, kindIn),
          Range(
            destStart + (overlapStart - sourceStart),
            overlapEnd - overlapStart + 1,
            kindOut
          ),
          Range(overlapEnd + 1, source.end - overlapEnd, kindIn)
        )
      } else {
        List(source)
      }
    } else {
      List(source)
    }
  }
}

case class IdentityConverter(
    override val kindIn: String,
    override val kindOut: String
) extends Converter {

  override def apply(source: Range) = {
    if (source.kind == kindIn) {
      List(Range(source.start, source.length, kindOut))
    } else {
      List(source)
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

    var convertersWithId: List[Converter] = converters
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

    // handle last ID converter which does not get appended with
    // the above logic using `sliding`
    convertersWithId = convertersWithId.appended(
      IdentityConverter(
        convertersWithId.last.kindIn,
        convertersWithId.last.kindOut
      )
    )

    val seedRanges = if (useRanges) {
      seedNumbers.grouped(2).map(pair => Range(pair(0), pair(1), "seed")).toSeq
    } else {
      seedNumbers.map(n => Range(n, 1, "seed"))
    }

    def singleConversion(ranges: Seq[Range], C: Converter) = {
      ranges.flatMap(C(_)).filter(_.length > 0)
    }
    val locationRanges = convertersWithId.foldLeft(seedRanges.toSeq)(singleConversion)
    println(locationRanges.map(_.start).min(math.Ordering.Long))
  }

  def test() = {
    val r1 = Range(1, 99, "s1")
    val r2 = Range(43, 1, "s1")
    val c1 = MappedConverter(1000, 30, 50, "s1", "s2")
    val ranges = c1(r1)
    println(ranges)

    println(c1(r2))

    val c2 = MappedConverter(1000, 100, 2, "s1", "s2")
    println(c2(r1))

    val c3 = IdentityConverter("s1", "s3")
    println(ranges.flatMap(c3(_)))
  }

  // test()

  // process(testFile)
  // process(testFile, true)
  // process(inputFile)
  process(inputFile, true)
}
