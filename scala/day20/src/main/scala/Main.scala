import scala.io.Source
import fastparse.*
import fastparse.NoWhitespace.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks

enum State:
  case Low
  case High

  def flip: State = this match
    case Low  => High
    case High => Low

case class Pulse(state: State, time: Int, to: String, from: String)
    extends Ordered[Pulse]:
  override def compare(that: Pulse): Int =
    1 * that.time.compare(this.time)

implicit def stateToInt: State => Int = {
  case State.Low  => 0
  case State.High => 1
}

val pulse_counter: mutable.HashMap[State, Int] = mutable.HashMap[State, Int]()
val queue: mutable.PriorityQueue[Pulse] = mutable.PriorityQueue()

trait Device:
  val label: String
  val dest_str: List[String]
  val dest: ListBuffer[Device]
  def pulse(pulse: Pulse): Unit =
    pulse_counter(pulse.state) += 1

case class Broadcaster(
    label: String,
    dest_str: List[String],
    dest: ListBuffer[Device]
) extends Device:
  override def pulse(pulse: Pulse): Unit = {
    super.pulse(pulse)
    dest.foreach(d =>
      queue.enqueue(Pulse(pulse.state, pulse.time + 1, d.label, this.label))
    )
  }

case class FlipFlop(
    label: String,
    dest_str: List[String],
    dest: ListBuffer[Device]
) extends Device:
  var state: State = State.Low

  override def pulse(pulse: Pulse): Unit = {
    super.pulse(pulse)
    pulse.state match
      case State.Low =>
        state = state.flip
        dest.foreach(d =>
          queue.enqueue(Pulse(state, pulse.time + 1, d.label, this.label))
        )
      case State.High =>
  }

case class Conjunction(
    label: String,
    dest_str: List[String],
    dest: ListBuffer[Device]
) extends Device:
  val inputs: mutable.Map[String, State] =
    collection.mutable.HashMap[String, State]()

  var n_inputs: Int = 0

  override def pulse(pulse: Pulse): Unit = {

    super.pulse(pulse)
    inputs.update(pulse.from, pulse.state)

    if inputs.values.map(stateToInt).sum() == n_inputs then
      dest.foreach(d =>
        queue.enqueue(Pulse(State.Low, pulse.time + 1, d.label, this.label))
      )
    else
      dest.foreach(d =>
        queue.enqueue(Pulse(State.High, pulse.time + 1, d.label, this.label))
      )
  }

case class Output(
    label: String,
    dest_str: List[String],
    dest: ListBuffer[Device]
) extends Device:
  override def pulse(pulse: Pulse): Unit = {
    super.pulse(pulse)
  }

def parse_input(file: String): List[Device] =

  def prefix[$: P] = CharIn("%&").!
  def label[$: P] = CharsWhileIn("a-zA-Z").!
  def device[$: P]: P[Tuple2[Option[String], String]] =
    P(prefix.? ~ label)
  def targets[$: P]: P[Seq[String]] = P(label.rep(sep = ", "))
  def entry[$: P]: P[Device] = P(device ~ " -> " ~ targets).map {
    case (None, "broadcaster", tl: List[String]) =>
      Broadcaster("broadcaster", tl, new ListBuffer[Device])
    case (Some("%"), label: String, tl) =>
      FlipFlop(label, tl.toList, new ListBuffer[Device])
    case (Some("&"), label: String, tl) =>
      Conjunction(label, tl.toList, new ListBuffer[Device])
    case _ => throw RuntimeException("Parsing error.")
  }

  val source = Source.fromFile(file)
  val lines = source.getLines().toList

  val results = lines.map(line => parse(line, c => entry(using c)))
  results.collect { case Parsed.Success(dev, _) =>
    dev
  }

def part1(file: String): Int =
  val devices = parse_input(file)

  val dmap = devices
    .map(d => d.label -> d)
    .appended("rx" -> Output("rx", List(), ListBuffer()))
    .appended("output" -> Output("output", List(), ListBuffer()))
    .toMap

  devices.foreach(d => {
    d.dest_str.foreach(l => {
      val target = dmap(l)
      d.dest.append(target)
    })
  })

  val inputs = devices
    .flatMap(d => d.dest_str.map(s => (s, d.label)))
    .groupBy(_(0))
    .map(g => g._1 -> g._2.map(_(1)))

  // pprint.pprintln(inputs)

  devices.foreach {
    case c: Conjunction =>
      c.n_inputs = inputs(c.label).length
    case _ =>
  }

  pulse_counter.addOne(State.Low, 0)
  pulse_counter.addOne(State.High, 0)
  // pprint.pprintln(devices)

  1 to 1000 foreach { n =>

    queue.enqueue(Pulse(State.Low, time = 0, "broadcaster", "button"))
    while (queue.nonEmpty) {
      val next_pulse = queue.dequeue()
      dmap(next_pulse.to).pulse(next_pulse)
    }
  }

  pulse_counter.values.product

def part2(file: String): Long =
  val devices = parse_input(file)

  val dmap = devices
    .map(d => d.label -> d)
    .appended("rx" -> Output("rx", List(), ListBuffer()))
    .appended("output" -> Output("output", List(), ListBuffer()))
    .toMap

  devices.foreach(d => {
    d.dest_str.foreach(l => {
      val target = dmap(l)
      d.dest.append(target)
    })
  })

  val inputs = devices
    .flatMap(d => d.dest_str.map(s => (s, d.label)))
    .groupBy(_(0))
    .map(g => g._1 -> g._2.map(_(1)))

  devices.foreach {
    case c: Conjunction =>
      c.n_inputs = inputs(c.label).length
    case _ =>
  }

  pulse_counter.addOne(State.Low, 0)
  pulse_counter.addOne(State.High, 0)

  val periods: mutable.Map[String, Long] = mutable.HashMap[String, Long]()

  Breaks.breakable {
    1 to 10000 foreach { n =>

      queue.enqueue(Pulse(State.Low, time = 0, "broadcaster", "button"))
      while (queue.nonEmpty) {
        val next_pulse = queue.dequeue()

        val ins = inputs("jm")
        ins.foreach(label => {
          if (next_pulse.from == label && next_pulse.state == State.High) {
            periods.update(label, n)
            if periods.size == 4 then Breaks.break
          }

        })

        dmap(next_pulse.to).pulse(next_pulse)
      }
    }
  }

  periods.values.product()

@main def main(): Unit =

  println(part1("test.txt"))
  println(part1("input.txt"))

  println(part2("input.txt"))
