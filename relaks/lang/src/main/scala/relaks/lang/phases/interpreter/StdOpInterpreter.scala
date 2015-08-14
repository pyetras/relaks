package relaks.lang.phases.interpreter

import relaks.lang.ast.{ScalaNumType, Stdlib, Apply, Expression}
import scalaz.Order
/**
 * Created by Pietras on 14.08.15.
 */
trait StdOpInterpreter extends BaseExprInterpreter with Stdlib {

  private def cmp(ord: scalaz.Ordering, op: Expression) = {
    import scalaz.Ordering._
    op match {
      case Stdlib.<= => ord != GT
      case Stdlib.>= => ord != LT
      case Stdlib.< => ord == LT
      case Stdlib.> => ord == GT
      case Stdlib.== => ord == EQ
    }
  }

  private val evalStdOp: PartialFunction[Expression, Any] = {
    case _/>Apply(Stdlib.CmpOp(op), (_/>lExpr) :: rExpr :: Nil) =>
      val l = evalExpression(lExpr)
      val r = evalExpression(rExpr)
      cmp(lExpr.tpe.asInstanceOf[ScalaNumType[Any]].order.apply(l, r), op)
  }

  override def evalExpression(expr: Expression): Any = evalStdOp.applyOrElse(expr, super.evalExpression)
}
