/**
 * Created by Pietras on 20/03/15.
 */

import fwb.parser.parsers.{PcParser, PrologParser}
import fwb.parser.printers.PrettyPrinter
import jpl._
import fwb.utils.prolog.PrologList._
object HelloTest {
  def main(args: Array[String]) {
//    JPL.setDefaultInitArgs(Array("swipl", "-g", "true", "-nosignals"))
//    var q = new Query("consult", Array[Term](new Atom("~/Documents/studia/mgr/fwb/src/parse.pl/scala")))
//    q.hasSolution()
//    JPL.getActualInitArgs().foreach(println _)
//    q = new Query("parse_str(\"x = x + 5 * \\\"hello\\\" - true;\", AST, Err)")
//    val result = q.oneSolution().get("AST").asInstanceOf[Term]
//    println(result)
//    val program:Program = new PrologParser().parse(result)
//    println(new PrettyPrinter(program).shows)

//    val p2 = new PcParser().parse(
//      """
//        x = 3.14
//      """)
//    println((new PrettyPrinter())(p2))
    import fwb.dsl.{Rep, DSL}

    object Program extends DSL
    import Program._


//    val i: Rep[Int] = 5
//    val j: Rep[Int] = 2
//    val k = choose between 10 and 20
//    val m = k + i
//    val l = once(m)
//    compile(l.tree)
  }
}