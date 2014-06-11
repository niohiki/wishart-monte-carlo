package org.niohiki.wishartmontecarlo

import scala.collection.mutable.ArrayBuffer
import org.niohiki.wishartmontecarlo.integrator._

case class FreeEnergy(result: Double, error: Double)
class FreeEnergyCalculator(system: System, beta: Double, t: Double)(
  implicit config: IntegratorConfiguration) {

  def apply(N: Int) = {
    val z = Integrate(integrand(N), N, system.domain(beta, t, N)).result
    z.map(Math.log(_))
  }
  private def integrand(N: Int): ArrayBuffer[Double] => Double = a => {
    var energy: Double = 0
    for (lambda <- a) {
      energy += system.V(lambda, beta, t, N) * N * beta / t
    }
    for (i <- 0 until a.length; j <- 0 until i) {
      energy -= 2 * beta * Math.log(Math.abs(a(i) - a(j)))
    }
    Math.exp(-energy)
  }
  private def factorial(n: Int): Double = if (n == 0) 1 else n * factorial(n - 1)
  private def normalization(n: Int) = Math.log(Math.pow(2 * Math.PI, n) * factorial(n))
}

abstract class System(implicit config: IntegratorConfiguration) {
  def V(lambda: Double, beta: Double, t: Double, N: Int): Double
  def domain(beta: Double, t: Double, N: Int): Domains.Domain
  def freeEnergy(beta: Double, t: Double) = new FreeEnergyCalculator(this, beta, t)
}
class Wishart(zeta: Double)(implicit config: IntegratorConfiguration) extends System {
  def domain(beta: Double, t: Double, N: Int) = Domains.Positives(zeta)
  val subLS = 0
  def V(lambda: Double, beta: Double, t: Double, N: Int) =
    lambda - (zeta - subLS * (beta - 1) / N) * Math.log(lambda)
}

class Gaussian(implicit config: IntegratorConfiguration) extends System {
  def domain(beta: Double, t: Double, N: Int) = Domains.Reals(0, 0.25 * Math.sqrt(beta * N / t))
  def V(lambda: Double, beta: Double, t: Double, N: Int) =
    0.5 * lambda * lambda
}