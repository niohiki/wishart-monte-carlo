package org.niohiki.wishartmontecarlo.integrator

import scala.collection.mutable.ArrayBuffer

case class Result(value: Double, error: Double) {
  def map(f: (Double) => (Double)) = {
    val nval = f(value)
    val nmax = f(value * (1 + error))
    val nmin = f(value * (1 - error))
    val nerr = Math.abs((nmax - nmin) / (2 * nval))
    Result(nval, nerr)
  }
}
class BinResult(from: Bins) {
  val under = from.normalizedUnder
  val over = from.normalizedOver
  val bins = from.normalizedBins
  override def toString = "(" + bins.
    map { case (value, point) => "(" + point + "," + value + ")" }.
    reduce(_ + "," + _) + ")"
}

class Bins(min: Double, max: Double, n: Int) {
  private var under: Double = 0
  private var over: Double = 0
  private val bins = ArrayBuffer.fill[Double](n)(0)
  private val step = (max - min) / n
  def add(value: Double, weight: Double) {
    val index = Math.floor((value - min) / step).toInt
    if (index < 0) under += weight
    else if (index >= bins.size) over += weight
    else bins(index) += weight
  }
  def midPoint(n: Int) = min + (n + 0.5) * step
  def norm = bins.reduce(_ + _) + over + under
  def normalizedBins: List[(Double, Double)] = {
    bins.toList.map(_ / norm).zip(Range(0, n).map(midPoint(_)))
  }
  def normalizedOver = over / norm
  def normalizedUnder = under / norm
}

case class IntegratorConfiguration(tolerance: Double, sampleSteps: Int, minSamples: Int,
  slaves: Int, verbose: Boolean)

case class BinnerConfiguration(min: Double, max: Double, bins: Int, samples: Int, verbose: Boolean)

object Domains {
  sealed trait Domain {
    def mapWithMeasure(integrand: Integrand): Integrand = vector => {
      val (nint, measure) = transformAndMeasure(integrand)(vector)
      nint * measure
    }
    def map(integrand: Integrand): Integrand = vector => {
      val (nint, measure) = transformAndMeasure(integrand)(vector)
      nint
    }
    def transformAndMeasure(integrand: Integrand): Integrand2
  }
  case class UnitInterval() extends Domain {
    def transformAndMeasure(integrand: Integrand) = vector => (integrand(vector), 1)
  }
  case class Reals(center: Double, width: Double) extends Domain {
    def transformAndMeasure(integrand: Integrand) = vector => {
      var measure: Double = 1
      var tan: Double = 0
      (integrand(vector.map(x => {
        tan = Math.tan(Math.PI * (x - 1.0 / 2))
        measure *= width * Math.PI * (tan * tan + 1)
        width * tan + center
      })), measure)
    }
  }
  case class Positives(width: Double) extends Domain {
    def transformAndMeasure(integrand: Integrand) = v => {
      var measure: Double = 1
      (integrand(v.map(x => {
        measure *= width / x
        -width * Math.log(x)
      })), measure)
    }
  }
}