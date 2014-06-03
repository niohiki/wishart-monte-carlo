package org.niohiki.wishartmontecarlo

import scala.collection.mutable.ArrayBuffer
import org.niohiki.wishartmontecarlo.integrator._

trait FreeEnergy {
  var beta: Double = 1
  var N: Int = 1
  var t: Double = 1
  def V(lambda: Double): Double
  def domain: Domains.Domain
  def integrand: ArrayBuffer[Double] => Double = a => {
    var energy: Double = 0
    for (lambda <- a) {
      energy += V(lambda) * N * beta / t
    }
    for (i <- 0 until a.length; j <- 0 until i) {
      energy -= 2 * beta * Math.log(Math.abs(a(i) - a(j)))
    }
    Math.exp(-energy)
  }
  def calculate = {
    implicit val config = IntegratorConfiguration(N, 0.1, 10000, 200, 2, false)
    Math.log(Integrator(integrand, domain)) - normalization(N)
  }
  def factorial(n: Int): Double = if (n == 0) 1 else n * factorial(n - 1)
  def normalization(n: Int) = Math.log(Math.pow(2 * Math.PI, n) * factorial(n))
}

class Wishart extends FreeEnergy {
  var zeta: Double = 1
  def domain = Domains.Positives(zeta)
  def V(lambda: Double) = lambda - (zeta - (beta - 1) / N) * Math.log(lambda)
}

class Gaussian extends FreeEnergy {
  def domain = Domains.Reals(0, 0.25 * Math.sqrt(beta * N))
  def V(lambda: Double) = 0.5 * lambda * lambda
}