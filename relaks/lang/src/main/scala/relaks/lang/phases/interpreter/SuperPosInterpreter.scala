package relaks.lang.phases.interpreter

import relaks.lang.ast.{Expression, NondetGeneratorList, NondetGeneratorRange, ScalaTypes}
import relaks.lang.dsl.extensions.{SuperPosAnalysis, SuperPosExtensions}
import relaks.optimizer.{ChooseOneOf, ContinuousRange, DiscreteRange, NondetParam}

import scalaz.EphemeralStream

/**
 * Created by Pietras on 13.08.15.
 */
trait SuperPosInterpreter extends SuperPosExtensions with SuperPosAnalysis { this: BaseExprInterpreter =>
  def evalSuperPosGenerator(expr: Expression): NondetParam[_] = expr match {
    case _/> NondetGeneratorRange(from, to) =>
      val frm = evalExpression(from)
      val t = evalExpression(to)

      expr.tpe match {
        case ScalaTypes.doubleType => new ContinuousRange(frm.asInstanceOf[Double], t.asInstanceOf[Double])
        case ScalaTypes.intType => new DiscreteRange(frm.asInstanceOf[Int], t.asInstanceOf[Int])
      }
    case _/> NondetGeneratorList(list) =>
      val lst = evalExpression(list)
      new ChooseOneOf[Any](lst.asInstanceOf[EphemeralStream[_]].toList)
  }
}
