
import scalaz._
import Scalaz._
import scala.reflect.runtime.{universe => ru}
import scala.language.implicitConversions

//import jpl._
//var q = new Query("consult", Array[Term](new Atom("~/Documents/studia/mgr/fwb/src/parse.pl/scala")))
//q.hasSolution()
//q = new Query("parse_str(\"x = 2 + 2;\", AST, Err)")
//val ast : Compound = q.oneSolution().get("AST").asInstanceOf[Compound]
//ast.args()(0).args()(1).args()(0)
sealed class Car
case class Fiat(small: Boolean) extends Car with Addable[Int]
case class Ferrari(doors: Int) extends Car

//def f(m:List[Car]) = m foreach ( _ match {
//  case Fiat(small) => println(small)
//  case Ferrari(doors) => println(doors + 2)
//})
trait Value{
  type ValueT
  val value: ValueT
}
object Value {
  def unapply(arg: Value) : Option[arg.ValueT] = {
    Some(arg.value)
  }
}
case class Constant(val value: Any) extends Value {
  type ValueT = Any
}
def x(a:Value) = a match {
  case Constant(x: Int) => x
  case _ => null
}
x(new Constant(5))
sealed trait Addable[T] {
  def add(x: T) : T = x
}
sealed case class Make(val make: String)
implicit def carToMake(f: Car) : Make = Make("Car")
implicit def fiatToMake(f: Fiat) : Make = Make("fiat")
implicit def ferrariToMake(f: Ferrari) : Make = Make("ferrari")
val c: Car = Fiat(true)
c : Make
new Fiat(true) :: new Ferrari(4) :: Nil
1 to 5
(1 until 1).toList
if (1 != 5) true else false
(1, "hello")
val t1 = ru.typeTag[Fiat].tpe
val t2 = ru.typeTag[{def add(x: Int): Int}].tpe
t1 <:< t2
ru.reify(2 + 2)
'sym
def print(str: String) = Reader((builder :StringBuilder) =>
  (builder ++= str, ()))
"""a\b"""
((x:(Int, String)) => {case (i, sb) => (i, sb)})