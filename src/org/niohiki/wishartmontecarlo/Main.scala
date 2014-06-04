package org.niohiki.wishartmontecarlo

import scala.util.Random
import org.niohiki.wishartmontecarlo.integrator._

object Main {
  def main(args: Array[String]) {
    implicit val config = IntegratorConfiguration(0.005, 100, 20, 2, false)
    val system = new Gaussian
    val F = system.freeEnergy(beta = 1.5, t = 1.2)
    println("Starting")
    val tim = System.nanoTime
    for (n <- 2 to 6) {
      val f = F.result(N = n)
      println("n=" + n + " F=" + f)
    }
    println("Done in " + ((System.nanoTime - tim) * 1e-9))
  }
}