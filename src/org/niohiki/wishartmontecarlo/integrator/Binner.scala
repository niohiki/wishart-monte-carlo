package org.niohiki.wishartmontecarlo.integrator

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Bin {
  def apply(function: Integrand, probability: Integrand, N: Int, dom: Domains.Domain)(
    implicit conf: BinnerConfiguration) = new IntervalBinner(
    dom.map(function), dom.mapWithMeasure(probability), N, conf)
}

class IntervalBinner(function: Integrand, probability: Integrand,
  N: Int, conf: BinnerConfiguration) {
  lazy val result = {
    val bins = new Bins(conf.min, conf.max, conf.bins)
    for (sample <- 0 to conf.samples) {
      if (conf.verbose && sample % 100000 == 1) println((sample - 1) + " samples generated")
      val vector = ArrayBuffer.fill(N)(0.0)
      for (i <- 0 until N) vector(i) = Random.nextDouble
      bins.add(function(vector), probability(vector))
    }
    new BinResult(bins)
  }
}