package org.niohiki.wishartmontecarlo.systems

import scala.collection.mutable.ArrayBuffer

import org.niohiki.wishartmontecarlo.integrator._

class FreeEnergyCalculator(system: System, beta: Double, t: Double)(
  implicit config: IntegratorConfiguration) {

  def apply(N: Int) = {
    val z = Integrate(system.probability(N, beta, t), N, system.domain(beta, t, N)).result
    z.map(Math.log(_))
  }
  private def factorial(n: Int): Double = if (n == 0) 1 else n * factorial(n - 1)
  private def normalization(n: Int) = Math.log(Math.pow(2 * Math.PI, n) * factorial(n))
}

class LargestEigenvalueCalculator(system: System, beta: Double, t: Double)(
  implicit config: BinnerConfiguration) {

  def apply(N: Int) = {
    Bin(v => v.max, system.probability(N, beta, t), N, system.domain(beta, t, N)).result
  }
}

abstract class System {
  def V(lambda: Double, beta: Double, t: Double, N: Int): Double
  def domain(beta: Double, t: Double, N: Int): Domains.Domain
  def freeEnergy(beta: Double, t: Double)(implicit config: IntegratorConfiguration) =
    new FreeEnergyCalculator(this, beta, t)
  def largestEigenvalue(beta: Double, t: Double)(implicit config: BinnerConfiguration) =
    new LargestEigenvalueCalculator(this, beta, t)
  def probability(N: Int, beta: Double, t: Double): ArrayBuffer[Double] => Double = a => {
    var energy: Double = 0
    for (lambda <- a) {
      energy += V(lambda, beta, t, N) * N * beta / t
    }
    for (i <- 0 until a.length; j <- 0 until i) {
      energy -= 2 * beta * Math.log(Math.abs(a(i) - a(j)))
    }
    Math.exp(-energy)
  }
}
class Wishart(zeta: Double, sl: Double) extends System {
  def domain(beta: Double, t: Double, N: Int) = Domains.Positives(zeta)
  def V(lambda: Double, beta: Double, t: Double, N: Int) =
    lambda - (zeta + sl * (beta - 1) / (beta * N)) * Math.log(lambda)
}

class Gaussian extends System {
  def domain(beta: Double, t: Double, N: Int) = Domains.Reals(0, 0.25 * Math.sqrt(beta * N / t))
  def V(lambda: Double, beta: Double, t: Double, N: Int) =
    0.5 * lambda * lambda
}