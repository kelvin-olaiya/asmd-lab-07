package u07.modelling

import java.util.Random
import u07.utils.Stochastics
import org.scalactic.Bool

object CTMCSimulation:

  case class Event[A](time: Double, state: A)
  type Trace[A] = LazyList[Event[A]]
  export CTMC.*

  extension [S](self: CTMC[S])
    def newSimulationTrace(s0: S, rnd: Random): Trace[S] =
      LazyList.iterate(Event(0.0, s0)):
        case Event(t, s) =>
          if self.transitions(s).isEmpty
          then Event(t, s)
          else
            val choices = self.transitions(s) map (t => (t.rate, t.state))
            val next = Stochastics.cumulative(choices.toList)
            val sumR = next.last._1
            val choice = Stochastics.draw(next)(using rnd)
            Event(t + Math.log(1 / rnd.nextDouble()) / sumR, choice)

    def traceUntil(using rnd: Random)(s0: S)(stopWhen: S => Boolean): Trace[S] =
      self
        .newSimulationTrace(s0, rnd)
        .takeWhile(e => !stopWhen(e.state))

    def nTracesUntil(using
        rnd: Random
    )(s0: S)(stopWhen: S => Boolean)(n: Int): Seq[Trace[S]] =
      LazyList
        .continually(self.traceUntil(s0)(stopWhen))
        .take(n)
        .toSeq

    def traceOfLength(using rnd: Random)(s0: S)(n: Int): Trace[S] =
      self.newSimulationTrace(s0, rnd).take(n)

    def nTracesOfLength(using
        rnd: Random
    )(s0: S)(n: Int)(m: Int): Seq[Trace[S]] =
      LazyList.continually(self.traceOfLength(s0)(n)).take(m).toSeq

    def traceUpTo(using rnn: Random)(s0: S)(state: S): Trace[S] =
      self.traceUntil(s0)(_ == state)

    def nTracesUpTo(using
        rnd: Random
    )(s0: S)(state: S)(n: Int): Seq[Trace[S]] =
      LazyList.continually(self.traceUpTo(s0)(state)).take(n).toSeq
