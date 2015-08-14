package relaks.lang

/**
 * Created by Pietras on 12.08.15.
 */
package object dsl {
  def branin(x: Double, y: Double) = {
    val sq: (Double => Double) = math.pow(_, 2)
    sq(y - (5.1/(4*sq(math.Pi)))*sq(x) + (5/math.Pi)*x - 6) + 10*(1-(1./(8*math.Pi)))*math.cos(x) + 10
  }
}
