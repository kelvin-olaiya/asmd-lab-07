package u07.examples

import u07.modelling.SPN
import u07.utils.MSet
import u07.modelling.SPN.Trn
import u07.modelling.SPN.toCTMC
import u07.modelling.CTMCSimulation.newSimulationTrace
import java.util.Random
import u07.modelling.CTMCSimulation.{traceUpTo, traceOfLength, nTracesOfLength}
import u07.utils.Statistics.averagePercentageOfTimeIn

object StochasticReadersAndWriters extends App:
  enum State:
    case IDLE, R, W, RC, WC, DONE
  import State.*

  def buildNetwork(
      toReaderRate: Double = 1.0,
      toWriterRate: Double = 1.0,
      readerWaitingRate: Double = 1.0,
      writerWaitingRate: Double = 1.0,
      readRate: Double = 1.0,
      writeRate: Double = 1.0
  ) =
    SPN(
      Trn(MSet(IDLE), m => toWriterRate, MSet(W), MSet()),
      Trn(MSet(IDLE), m => toReaderRate, MSet(R), MSet()),
      Trn(MSet(R), m => readerWaitingRate, MSet(RC), MSet(WC)),
      Trn(MSet(W), m => readerWaitingRate, MSet(WC), MSet(RC, WC)),
      Trn(MSet(RC), m => m(RC) * readRate, MSet(DONE), MSet()),
      Trn(MSet(WC), m => writeRate, MSet(DONE), MSet()),
      Trn(MSet(DONE), m => 1.0, MSet(IDLE), MSet())
    )

  val basicNetwork = buildNetwork()
  val moreReadersThanWriters = buildNetwork(
    toReaderRate = 7500,
    toWriterRate = 2500
  )
  val moreWritersThanReaders = buildNetwork(
    toReaderRate = 2500,
    toWriterRate = 7500
  )
  val longRead = buildNetwork(readRate = 0.25)
  val longWrite = buildNetwork(writeRate = 0.25)
  val priorityToReaders = buildNetwork(
    readerWaitingRate = 8000,
    writerWaitingRate = 2000
  )
  val priorityToWriters = buildNetwork(
    readerWaitingRate = 2000,
    writerWaitingRate = 8000
  )
  val priorityToWritersAndLongRead = buildNetwork(
    readerWaitingRate = 2000,
    writerWaitingRate = 8000,
    readRate = 0.25
  )
  val priorityToReadersAndLongWrite = buildNetwork(
    readerWaitingRate = 8000,
    writerWaitingRate = 2000,
    writeRate = 0.25
  )
  val priorityToReadersAndLongRead = buildNetwork(
    readerWaitingRate = 8000,
    writerWaitingRate = 2000,
    readRate = 0.25
  )
  val priorityToWritersAndLongWrite = buildNetwork(
    readerWaitingRate = 2000,
    writerWaitingRate = 8000,
    writeRate = 0.25
  )
  val moreReadersThanWritersAndLongRead = buildNetwork(
    toReaderRate = 7500,
    toWriterRate = 2500,
    readRate = 0.25
  )
  val moreWritersThanReadersAndLongWrite = buildNetwork(
    toReaderRate = 2500,
    toWriterRate = 7500,
    writeRate = 0.25
  )
  val moreReadersThanWritersAndPriorityToReaders = buildNetwork(
    toReaderRate = 7500,
    toWriterRate = 2500,
    readerWaitingRate = 8000,
    writerWaitingRate = 2000
  )
  val moreWritersThanReadersAndPriorityToWriters = buildNetwork(
    toReaderRate = 2500,
    toWriterRate = 7500,
    readerWaitingRate = 2000,
    writerWaitingRate = 8000
  )
  val moreReadersThanWritersAndPriorityToWriters = buildNetwork(
    toReaderRate = 7500,
    toWriterRate = 2500,
    readerWaitingRate = 2000,
    writerWaitingRate = 8000
  )
  val moreWritersThanReadersAndPriorityToReaders = buildNetwork(
    toReaderRate = 2500,
    toWriterRate = 7500,
    readerWaitingRate = 8000,
    writerWaitingRate = 2000
  )

  given Random = new Random()
  println:
    toCTMC(basicNetwork)
      .nTracesOfLength(MSet(IDLE, IDLE, IDLE, IDLE, IDLE))(1000)(1)
      .averagePercentageOfTimeIn(m => m matches MSet(W))
