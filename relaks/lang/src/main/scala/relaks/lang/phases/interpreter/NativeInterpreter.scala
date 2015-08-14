package relaks.lang.phases.interpreter

import relaks.lang.ast._
import relaks.lang.dsl.extensions.TupleInterpreter
import relaks.lang.dsl.extensions.ast.Symbols
import relaks.lang.impl.Row
import scala.reflect.runtime.{universe => ru}

/**
 * Created by Pietras on 13.08.15.
 */
trait NativeInterpreter extends BaseExprInterpreter with Symbols with TupleInterpreter {
  private val evalNativeCall: PartialFunction[Expression, Any] = {
    case _/>ApplyNative(fn, argsExpr) =>
      val args = evalTupleExpression(argsExpr)
      val ref = ru.runtimeMirror(ru.getClass.getClassLoader).reflect(fn)
      val apply = ref.symbol.typeSignature.member(ru.TermName("apply"))
      val result = ref.reflectMethod(apply.asMethod)(args.values:_*).asInstanceOf[Expression]
      evalExpression(result)
    case _/>Native(value) => value
  }

  override def evalExpression(expr: Expression): Any = evalNativeCall applyOrElse(expr, super.evalExpression)
}
