package u07.utils

import u07.modelling.CTMCSimulation.Trace
import u07.modelling.CTMCSimulation.Event
import org.scalactic.Bool

object Statistics:

  extension [A](trace: Trace[A])
    def time: Double = trace.lastOption.map(_.time).getOrElse(0.0)
    def timeUntil(condition: A => Boolean): Option[Double] =
      trace
        .find(e => condition(e.state))
        .map(_.time)
    def timeTo(to: A): Option[Double] = timeUntil(_ == to)
    def timeOfNth(n: Int): Double = trace(n - 1).time

    def timeIn(state: A): Double = timeIn(_ == state)
    def timeIn(condition: A => Boolean): Double =
      val result = trace.reverse.foldLeft((0.0, trace.last.time)):
        case ((time, prevEvTime), event) =>
          condition(event.state) match
            case true  => (time + (prevEvTime - event.time), event.time)
            case false => (time, event.time)
      result._1

    def percentageOfTimeIn(condition: A => Boolean): Double =
      timeIn(condition) / time
    def percentageOfTimeIn(state: A): Double = percentageOfTimeIn(_ == state)

  extension [A](traces: Seq[Trace[A]])
    def averageTime: Double = traces.map(_.time).sum / traces.size
    def averageTimeTo(condition: A => Boolean): Option[Double] =
      val tracesToConsider =
        traces.filter(t => t.exists(e => condition(e.state)))
      Some(tracesToConsider.map(_.timeUntil(condition).get).sum)
        .filter(_ => !tracesToConsider.isEmpty)
        .map(_ / tracesToConsider.size)
    def averageTimeTo(state: A): Option[Double] = averageTimeTo(_ == state)
    def averageTimeIn(condition: A => Boolean): Double =
      traces.map(_.timeIn(condition)).sum / traces.size
    def averageTimeIn(state: A): Double = averageTimeIn(_ == state)
    def averageTimeToNth(n: Int): Double =
      traces.map(_.timeOfNth(n)).sum / traces.size
    def averagePercentageOfTimeIn(condition: A => Boolean): Double =
      traces.map(_.percentageOfTimeIn(condition)).sum / traces.size
    def averagePercentageOfTimeIn(state: A): Double =
      averagePercentageOfTimeIn(_ == state)
