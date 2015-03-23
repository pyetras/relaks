/**
 * Created by Pietras on 20/03/15.
 */

import fwb.parser.ast.ASTNode
import fwb.parser.ast.Program.Program
import jpl._
import fwb.utils.prolog.PrologList._
object HelloTest {
  def main(args: Array[String]) {
//    JPL.setDefaultInitArgs(Array("swipl", "-g", "true", "-nosignals"))
    var q = new Query("consult", Array[Term](new Atom("~/Documents/studia/mgr/fwb/src/parse.pl/scala")))
    q.hasSolution()
//    JPL.getActualInitArgs().foreach(println _)
    q = new Query("parse_str(\"x = x + x * [2, 3] + (true + \\\"hello\\\");\", AST, Err)")
    val result = q.oneSolution().get("AST").asInstanceOf[Term]
    println(result)
    val program:Program = result.map(ASTNode(_))
    println(program)
  }

}