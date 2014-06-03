package org.niohiki.wishartmontecarlo

import scala.util.Random
import org.niohiki.wishartmontecarlo.integrator._

object Main {
  def main(args: Array[String]) {
    val system = new Wishart
    system.beta = 1
    system.t = 1
    system.zeta = 1
    println("Starting")
    val tim = System.nanoTime
    for (n <- 2 to 10) {
      system.N = n
      val f = system.calculate
      println("n=" + n + " F=" + f)
    }
    println("Done in " + ((System.nanoTime - tim) * 1e-9))
  }
}