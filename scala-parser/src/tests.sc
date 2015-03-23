
//import jpl._
//var q = new Query("consult", Array[Term](new Atom("~/Documents/studia/mgr/fwb/src/parse.pl/scala")))
//q.hasSolution()
//q = new Query("parse_str(\"x = 2 + 2;\", AST, Err)")
//val ast : Compound = q.oneSolution().get("AST").asInstanceOf[Compound]
//ast.args()(0).args()(1).args()(0)
sealed trait Car
case class Fiat(small: Boolean) extends Car
case class Ferrari(doors: Int) extends Car

def f(m:List[Car]) = m foreach ( _ match {
  case Fiat(small) => println(small)
  case Ferrari(doors) => println(doors + 2)
})

new Fiat(true) :: new Ferrari(4) :: Nil
1 to 5
(1 until 1).toList
if (1 != 5) true else false

(1, "hello")