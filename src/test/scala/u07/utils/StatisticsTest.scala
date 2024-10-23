package u07.utils

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import u07.modelling.CTMCSimulation.Event
import u07.utils.Statistics.{
  time,
  timeIn,
  timeTo,
  timeOfNth,
  percentageOfTimeIn
}
import u07.modelling.CTMCSimulation.Trace
import u07.utils.Statistics.averagePercentageOfTimeIn
import u07.utils.Statistics.averageTimeTo
import u07.utils.Statistics.averageTime

class StatisticsTest extends AnyFunSuite with Matchers:
  enum State:
    case A, B, C, D, E
  import State.*

  private def traceOf[T](events: (Double, T)*): Trace[T] =
    LazyList.from(events.map(Event.apply))

  test("Measurement of the time of a simulation trace"):
    val trace: Trace[State] = traceOf((0.0, A), (1.0, B), (2.0, A))
    trace.time shouldBe 2.0

  test("Measurement of the time spent in a particular state"):
    val trace: Trace[State] = traceOf((0.0, A), (1.0, B), (2.0, A), (3.0, B))
    trace.timeIn(A) shouldBe 2.0
    trace.timeIn(B) shouldBe 1.0

  test("Measurment of the time spent to reach a particular state"):
    val trace: Trace[State] =
      traceOf((0.0, A), (1.0, B), (2.0, A), (3.0, B), (4.0, C), (5.0, D))
    trace.timeTo(A) shouldBe Some(0.0)
    trace.timeTo(B) shouldBe Some(1.0)
    trace.timeTo(C) shouldBe Some(4.0)
    trace.timeTo(D) shouldBe Some(5.0)
    trace.timeTo(E) shouldBe None

  test("Measurment of the time spent reaching the nth event"):
    val trace: Trace[State] =
      traceOf((0.0, A), (1.0, B), (2.0, A))
    trace.timeOfNth(1) shouldBe 0.0
    trace.timeOfNth(2) shouldBe 1.0
    assertThrows[IndexOutOfBoundsException]:
      trace.timeOfNth(4)

  test("Measurement of the percentage of time spent in a particular state"):
    val trace: Trace[State] = traceOf((0.0, A), (1.0, B), (2.0, A), (3.0, B))
    trace.percentageOfTimeIn(A) shouldBe (0.66 +- 0.05)

  test(
    "Measurment of the average percentage of time spent in a particular state"
  ):
    val traces = Seq(
      traceOf((0.0, A), (1.0, B), (2.0, A), (3.0, B)),
      traceOf((0.0, A), (1.0, B), (2.0, A), (3.0, B), (4.0, C))
    )
    traces.averagePercentageOfTimeIn(A) shouldBe (0.58 +- 0.02)
    traces.averagePercentageOfTimeIn(E) shouldBe 0.0

  test("Measurment of the average to reach a particular state"):
    val traces = Seq(
      traceOf((0.0, A), (1.0, B), (2.0, A), (3.0, C), (5.0, E)),
      traceOf((0.0, A), (1.0, C), (2.0, A), (3.0, B), (4.0, C))
    )
    traces.averageTimeTo(B) shouldBe Some(2.0)
    traces.averageTimeTo(C) shouldBe Some(2.0)
    traces.averageTimeTo(D) shouldBe None
    traces.averageTimeTo(E) shouldBe Some(5.0)

  test("Measurment of the average time of n simulations"):
    val traces = Seq(
      traceOf((0.0, A), (1.0, B), (2.0, A), (3.0, C)),
      traceOf((0.0, A), (1.0, C), (2.0, A), (3.0, B), (4.0, C))
    )
    traces.averageTime shouldBe 3.5
