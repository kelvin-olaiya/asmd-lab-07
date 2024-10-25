package u07.benchmark

import u07.examples.StochasticReadersAndWriters.basicNetwork
import u07.modelling.SPN.toCTMC
import u07.modelling.CTMCSimulation.traceOfLength
import u07.utils.MSet
import u07.examples.StochasticReadersAndWriters.State.*
import java.util.Random
import u07.modelling.CTMCSimulation.nTracesOfLength
import u07.examples.StochasticReadersAndWriters.*
import u07.utils.Statistics.averagePercentageOfTimeIn

object StochasticRWBenchmark extends App:
  given Random = new Random()

  extension (n: Int) def procs = MSet((Array.fill(n)(IDLE))*)

  val networks = Seq(
    "Basic" -> basicNetwork,
    "More readers than writers" -> moreReadersThanWriters,
    "More writers than readers" -> moreWritersThanReaders,
    "Long read" -> longRead,
    "Long write" -> longWrite,
    "Priority to readers" -> priorityToReaders,
    "Priority to writers" -> priorityToWriters,
    "Priority to writers and long read" -> priorityToWritersAndLongRead,
    "Priority to readers and long write" -> priorityToReadersAndLongWrite,
    "Priority to readers and long read" -> priorityToReadersAndLongRead,
    "Priority to writers and long write" -> priorityToWritersAndLongWrite,
    "More readers than writers and long read" -> moreReadersThanWritersAndLongRead,
    "More writers than readers and long write" -> moreWritersThanReadersAndLongWrite,
    "More readers than writers and priority to readers" -> moreReadersThanWritersAndPriorityToReaders,
    "More writers than readers and priority to writers" -> moreWritersThanReadersAndPriorityToWriters,
    "More readers than writers and priority to writers" -> moreReadersThanWritersAndPriorityToWriters,
    "More writers than readers and priority to readers" -> moreWritersThanReadersAndPriorityToReaders
  )

  for (name, network) <- networks do
    val traces = toCTMC(network).nTracesOfLength(5.procs)(1000)(1000)

    println(s"Network: $name")
    println(
      s"  Average percentate of time READING: ${traces.averagePercentageOfTimeIn(_ matches MSet(RC))}"
    )
    println(
      s"  Average percentage of time WRITING: ${traces.averagePercentageOfTimeIn(_ matches MSet(WC))}"
    )
    println("---------------------------------------------------------------")
