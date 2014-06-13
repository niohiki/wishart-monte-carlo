package org.niohiki.wishartmontecarlo.largesteigenvalue

import org.niohiki.wishartmontecarlo.integrator.Bins
import org.niohiki.wishartmontecarlo.integrator.Binner
import org.niohiki.wishartmontecarlo.integrator.BinnerConfiguration
import org.niohiki.wishartmontecarlo.integrator.Integrand
import org.niohiki.wishartmontecarlo.integrator.Domains

object Main {
  def main(args: Array[String]) {
    implicit val conf = BinnerConfiguration(0, 10, 10, 500000)
    val function: Integrand = v => {
      v.reduce(_ + _)
    }
    val probability: Integrand = v => {
      Math.exp(-Math.pow(Math.abs(v.reduce(_ + _)), 2) * 0.05)
    }
    val r = Binner(function, probability, 1, Domains.Positives(1))
    println(r.result.toString.replaceAllLiterally("(", "{").replaceAllLiterally(")", "}"))
  }
}