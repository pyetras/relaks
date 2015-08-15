package relaks.lang.phases.interpreter

import relaks.lang.ast._
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
    case _/>Apply(Stdlib.CmpOp(op), lExpr :: rExpr :: Nil) =>
      val l = evalExpression(lExpr)
      val r = evalExpression(rExpr)
      cmp(lExpr.tpe.asInstanceOf[ScalaType[Any]].order.apply(l, r), op)

    case _/>Apply(Stdlib.NumericOp(op), lExpr :: rExpr :: Nil) =>
      val l = evalExpression(lExpr)
      val r = evalExpression(rExpr)
      val field = lExpr.tpe.asInstanceOf[ScalaNumType[Any]].field
      op match {
        case Stdlib.+ => field.+(l, r)
        case Stdlib.- => field.-(l, r)
        case Stdlib./ => field./(l, r)
        case Stdlib.* => field.*(l, r)
      }
  }

  override def evalExpression(expr: Expression): Any = evalStdOp.applyOrElse(expr, super.evalExpression)
}
