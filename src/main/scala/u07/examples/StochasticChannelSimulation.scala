package u07.examples

import u07.utils.Time
import java.util.Random
import u07.examples.StochasticChannel.*
import scala.runtime.Statics
import u07.utils.Statistics.averageTime
import u07.utils.Statistics.averageTimeIn
import u07.utils.Statistics.averagePercentageOfTimeIn

@main def mainStochasticChannelSimulation =
  given Random = new Random()
  val traces = stocChannel.nTracesUpTo(IDLE)(DONE)(1000)

  println(traces.averageTime)
  println(traces.averagePercentageOfTimeIn(FAIL))

  Time.timed:
    println:
      stocChannel
        .newSimulationTrace(IDLE, new Random)
        .take(10)
        .toList
        .mkString("\n")
