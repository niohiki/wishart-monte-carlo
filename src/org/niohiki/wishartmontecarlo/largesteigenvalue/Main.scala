package org.niohiki.wishartmontecarlo.largesteigenvalue

import org.niohiki.wishartmontecarlo.integrator.Bins
import org.niohiki.wishartmontecarlo.integrator.Bin
import org.niohiki.wishartmontecarlo.integrator.BinnerConfiguration
import org.niohiki.wishartmontecarlo.integrator.Integrand
import org.niohiki.wishartmontecarlo.integrator.Domains
import org.niohiki.wishartmontecarlo.systems.Wishart
import java.io.PrintWriter
import java.io.File

object Main {
  def generateFile(beta: Double, t: Double) {
    val zeta = 1
    val N = 10
    println("Starting beta=" + beta + " t=" + t)
    val tim = System.nanoTime
    implicit val conf = BinnerConfiguration(0, 15, 10, 100000000, true)
    val wishart = new Wishart(zeta = 1, sl = 1)
    val maxeig = wishart.largestEigenvalue(beta = beta, t = t)
    val bins = maxeig(N)
    val output = new PrintWriter(new File("le_t" + t + "beta" + beta + ".csv"))
    bins.bins.foreach {
      case (value, point) => output.println(point + "," + value)
    }
    output.close
    println("Done in " + ((System.nanoTime - tim) * 1e-9))
  }
  def main(args: Array[String]) {
    for (beta <- List(0.5, 1.0, 1.1, 1.5); t <- List(0.5, 1.0, 1.5)) {
      generateFile(beta, t)
    }
  }
}