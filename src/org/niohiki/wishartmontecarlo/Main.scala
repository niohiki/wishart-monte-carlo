package org.niohiki.wishartmontecarlo

import scala.util.Random
import org.niohiki.wishartmontecarlo.integrator._
import scala.io.Source
import java.io.PrintWriter
import java.io.File

object Main {
  def main(args: Array[String]) {
    val beta = 2
    val t = 1.5
    val zeta = 1
    val maxN = 5

    println("Starting")
    val tim = System.nanoTime

    implicit val config = IntegratorConfiguration(0.01, 500, 50, 2, false)
    val F = new Wishart(zeta).freeEnergy(beta = beta, t = t)
    val G = new Gaussian().freeEnergy(beta = beta, t = t)
    val output = new PrintWriter(new File("t" + t + "beta" + beta + ".csv"))
    for (n <- 1 to maxN) {
      lazy val f = F(N = n)
      lazy val g = G(N = n)
      output.println(n+","+f.value + "," + f.error + "," + g.value + "," + g.error)
      println("n=" + n + " F-G=" + (f.value - g.value) + " e(F)=" + f.error)
    }
    output.close
    println("Done in " + ((System.nanoTime - tim) * 1e-9))
  }
}