package org.niohiki.wishartmontecarlo

import scala.collection.mutable.ArrayBuffer
package object integrator {
  type Integrand = ArrayBuffer[Double] => Double
  type Integrand2 = ArrayBuffer[Double] => (Double, Double)
}