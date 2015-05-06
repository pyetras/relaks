package relaks.optimizer

import scala.reflect.runtime.universe._
import scala.language.dynamics

/**
 * Created by Pietras on 23/04/15.
 */
trait NondetParams extends NondetParamTypes {
  type ParamsSpace = Map[String, NondetParamType[Any]]
  type Params = Map[String, Any]

  case class Nondet(name: String, value: NondetParamType[Any])

  case class ParamProvider(paramSpace: ParamsSpace) extends Dynamic {
    def selectDynamic(name: String): Nondet = Nondet(name, paramSpace(name))
  }

}