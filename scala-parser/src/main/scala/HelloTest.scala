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
    val x = choose between 5 and 10
    val y = choose from List(1, 3, 4)
    val a = x + 5
    println(showSpace(a))
    List(x, y)

  }

}