package relaks.lang

/**
 * Created by Pietras on 12.08.15.
 */
package object dsl {
  def branin(x: Double, y: Double) = {
    val a = x*15
    val b = y*15-5
    val sq: Double => Double = math.pow(_, 2)

    sq(b - (5.1/(4*sq(math.Pi)))*sq(a) + (5/math.Pi)*a - 6) + 10*(1-(1./(8*math.Pi)))*math.cos(a) + 10
  }
}
