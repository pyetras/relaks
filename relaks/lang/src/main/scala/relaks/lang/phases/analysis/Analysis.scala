package relaks.lang.phases.analysis

import org.kiama.relation.DAGIr
import org.kiama.rewriting.Rewriter
import relaks.lang.ast.Expression

import scalaz.Scalaz._
import scalaz._

/**
 * Created by Pietras on 12/07/15.
 */
trait Analysis {
  final def analyze(root: Expression) = {
    val ir = new DAGIr(root)
    val strategy = Rewriter.collect[List, ValidationNel[String, Unit]] ({
      case (expr:Expression) => doAnalyze(expr, ir)
    })

    strategy(root).reduce { _ *> _}
  }
  protected def doAnalyze(root: Expression, ir: DAGIr): ValidationNel[String, Unit] =
    ().successNel[String]
}
