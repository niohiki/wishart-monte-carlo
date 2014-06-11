package org.niohiki.wishartmontecarlo.integrator

import scala.util.Random
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.collection.mutable.ArrayBuffer

case class Result(value: Double, error: Double) {
  def map(f: (Double) => (Double)) = {
    val nval = f(value)
    val nmax = f(value * (1 + error))
    val nmin = f(value * (1 - error))
    val nerr = Math.abs((nmax - nmin) / (2 * nval))
    Result(nval, nerr)
  }
}
case class IntegratorConfiguration(tolerance: Double, sampleSteps: Int, minSamples: Int,
  slaves: Int, verbose: Boolean)

object Domains {
  sealed trait Domain {
    def transform(integrand: Integrand): Integrand
  }
  case class UnitInterval() extends Domain {
    def transform(integrand: Integrand) = integrand
  }
  case class Reals(center: Double, width: Double) extends Domain {
    def transform(integrand: Integrand) = vector => {
      var measure: Double = 1
      var tan: Double = 0
      integrand(vector.map(x => {
        tan = Math.tan(Math.PI * (x - 1.0 / 2))
        measure *= width * Math.PI * (tan * tan + 1)
        width * tan + center
      })) * measure
    }
  }
  case class Positives(width: Double) extends Domain {
    def transform(integrand: Integrand) = v => {
      var measure: Double = 1
      integrand(v.map(x => {
        measure *= width / x
        -width * Math.log(x)
      })) * measure
    }
  }
}

object Integrate {
  def apply(integrand: Integrand, N: Int, dom: Domains.Domain)(implicit conf: IntegratorConfiguration) =
    new IntervalIntegration(dom.transform(integrand), N, conf)
}

class IntervalIntegration(integrand: Integrand, N: Int, conf: IntegratorConfiguration) {
  private def sample: Double = {
    var sum: Double = 0
    var steps = 0
    val vector = ArrayBuffer.fill(N)(0.0)
    while (steps < conf.sampleSteps) {
      for (i <- 0 until N) vector(i) = Random.nextDouble
      sum += integrand(vector)
      steps += 1
    }
    sum / steps
  }
  lazy val result = {
    var samples = 0
    var mean: Double = 0
    var pmean: Double = 0
    var err: Double = 0
    var S: Double = 0
    var M: Double = 0
    var logerr: Double = 0
    var errlevel: Double = 0
    val toleranceSqr = Math.pow(conf.tolerance, 2)
    def update(next: Double) {
      this.synchronized {
        S += next; samples += 1
        pmean = mean; mean = S / samples
        M += (next - pmean) * (next - mean)
        err = Math.abs(M / (S * S - S * mean))
      }
    }
    while (err > toleranceSqr || samples < conf.minSamples) {
      val values = List.tabulate(conf.slaves)(i => future { sample })
      values foreach (_ onSuccess {
        case value => update(value)
      })
      update(sample)
      if (conf.verbose) {
        logerr = -Math.log10(err)
        if (logerr > errlevel + 1 / (errlevel * errlevel + 1)) {
          errlevel += 1 / (errlevel * errlevel + 1)
          println("V=" + mean + "@" + Math.sqrt(err))
        }
      }
    }
    if (conf.verbose) println("Used " + samples + " samples")
    Result(mean, Math.sqrt(err))
  }
}
    