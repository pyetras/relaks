package relaks.lang.ast

/**
 * Created by Pietras on 15/04/15.
 */
import scalaz.Scalaz._

trait Stdlib {

  object Stdlib {
    val + = Operator("+")
    val - = Operator("-")
    val * = Operator("*")
    val / = Operator("/")
    val < = Operator("<")
    val <= = Operator("<=")
    val > = Operator(">")
    val >= = Operator(">=")
    val == = Operator("==")
    val ! = Operator("!")
    val || = Operator("||")
    val && = Operator("&&")
    val at = Operator("[")

    object CmpOp {
      def unapply(expression: Expression) = expression match {
        case Stdlib.< => expression.some
        case Stdlib.<= => expression.some
        case Stdlib.> => expression.some
        case Stdlib.>= => expression.some
        case _ => None
      }
    }
  }

}
