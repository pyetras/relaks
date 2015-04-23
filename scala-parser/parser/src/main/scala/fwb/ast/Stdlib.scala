package fwb.ast

/**
 * Created by Pietras on 15/04/15.
 */
trait Stdlib { this: ASTNodes =>

  object Stdlib {
    val + = Operator("+")
    val - = Operator("-")
    val * = Operator("*")
    val / = Operator("/")
    val < = Operator("<")
    val <= = Operator("<=")
    val > = Operator(">")
    val == = Operator("==")
    val ! = Operator("!")
    val || = Operator("||")
    val && = Operator("&&")
    val at = Operator("[")
  }

}
