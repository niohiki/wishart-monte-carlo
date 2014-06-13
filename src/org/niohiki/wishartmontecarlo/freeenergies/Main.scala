package org.niohiki.wishartmontecarlo.freeenergies

import org.niohiki.wishartmontecarlo.integrator._
import java.io.PrintWriter
import java.io.File
import org.niohiki.wishartmontecarlo.systems.Wishart
import org.niohiki.wishartmontecarlo.systems.Gaussian

object Main {
  def generateFile(beta: Double, t: Double, sl: Double) {
    val zeta = 1
    val maxN = 5

    println("Starting beta=" + beta + " t=" + t + " sl=" + sl)
    val tim = System.nanoTime

    implicit val config = IntegratorConfiguration(0.01, 250, 50, 2, false)
    val F = new Wishart(zeta, sl).freeEnergy(beta = beta, t = t)
    val G = new Gaussian().freeEnergy(beta = beta, t = t)
    val output = new PrintWriter(new File("fe_t" + t + "beta" + beta + "sl" + sl + ".csv"))
    for (n <- 1 to maxN) {
      lazy val f = F(N = n)
      lazy val g = G(N = n)
      output.println(n + "," + f.value + "," + f.error + "," + g.value + "," + g.error)
      println("n=" + n + " F-G=" + (f.value - g.value) + " e(F)=" + f.error)
    }
    output.close
    println("Done in " + ((System.nanoTime - tim) * 1e-9))
  }
  def main(args: Array[String]) {
    for (beta <- List(0.5, 1.0, 1.5, 2.0); t <- List(0.5, 1.0, 1.5); ls <- List(0.0, 1.0)) {
      generateFile(beta, t, ls)
    }
  }
}