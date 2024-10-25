package u07.examples

import scala.util.Try
import de.sciss.chart.api._
import org.jfree.data.xy.XYSeriesCollection
import org.jfree.data.xy.XYSeries
import javax.sound.midi.Track

object Brussellator extends App:
  import u07.modelling.{CTMC, SPN}
  import u07.utils.MSet
  import java.util.Random

  enum Place:
    case A, B, X, Y, D, E

  export Place.*
  export u07.modelling.CTMCSimulation.*
  export u07.modelling.SPN.*

  val spn = SPN[Place](
    Trn(MSet(A), m => 1.0 * m(A), MSet(X), MSet()),
    Trn(MSet(X, X, Y), m => 1.0 * (m(X) * m(Y)), MSet(X, X, X), MSet()),
    Trn(MSet(B, X), m => 1.0 * (m(B) * m(X)), MSet(Y, D), MSet()),
    Trn(MSet(X), m => 1.0 * m(X), MSet(E), MSet()),
    Trn(MSet(), m => 1.0, MSet(A, B), MSet())
  )

  extension (n: Int) infix def of(s: Place): Array[Place] = Array.fill(n)(s)
  extension (s: XYSeries)
    def from(points: Iterable[(Double, Double)]): XYSeries =
      points.foreach(p => s.add(p._1, p._2))
      s

  val trace = toCTMC(spn)
    .newSimulationTrace(
      MSet(((100 of X) ++ (100 of Y) ++ (100 of A) ++ (300 of B))*),
      new Random
    )
    .take(20000)

  val series = XYSeriesCollection()
  Seq("[X]" -> X, "[Y]" -> Y).foreach: (name, place) =>
    series.addSeries:
      XYSeries(name).from:
        trace.map(e => (e.time, e.state(place)))
  XYLineChart(series).show()
