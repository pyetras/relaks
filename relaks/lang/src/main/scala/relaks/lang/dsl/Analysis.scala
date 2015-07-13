package relaks.lang.dsl

import org.kiama.rewriting.Rewriter
import relaks.lang.ast.Expression

import scalaz._
import Scalaz._

/**
 * Created by Pietras on 12/07/15.
 */
trait Analysis {
  final def analyze(root: Expression) = {
    val strategy = Rewriter.collect[List, ValidationNel[String, Unit]] ({
      case (expr:Expression) => doAnalyze(expr)
    })

    strategy(root).reduce { _ *> _}
  }
  protected def doAnalyze(root: Expression): ValidationNel[String, Unit] =
    ().successNel[String]

}
