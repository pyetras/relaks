package relaks.lang.phases.interpreter

import relaks.lang.ast.{Expression, HyperparamList, HyperparamRange, ScalaTypes}
import relaks.lang.dsl.extensions.{HyperparamAnalysis, HyperparamExtensions}
import relaks.optimizer.{ChooseOneOf, ContinuousRange, DiscreteRange, NondetParam}

import scalaz.EphemeralStream

/**
 * Created by Pietras on 13.08.15.
 */
trait HyperparamInterpreter extends HyperparamExtensions with HyperparamAnalysis { this: BaseExprInterpreter =>
  def evalSuperPosGenerator(expr: Expression): NondetParam[_] = expr match {
    case _/> HyperparamRange(from, to) =>
      val frm = evalExpression(from)
      val t = evalExpression(to)

      expr.tpe match {
        case ScalaTypes.doubleType => new ContinuousRange(frm.asInstanceOf[Double], t.asInstanceOf[Double])
        case ScalaTypes.intType => new DiscreteRange(frm.asInstanceOf[Int], t.asInstanceOf[Int])
        case _ => throw new NotImplementedError("Hyperparam range other than int and double not implemented")
      }
    case _/> HyperparamList(list) =>
      val lst = evalExpression(list)
      new ChooseOneOf[Any](lst.asInstanceOf[EphemeralStream[_]].toList)
  }
}
