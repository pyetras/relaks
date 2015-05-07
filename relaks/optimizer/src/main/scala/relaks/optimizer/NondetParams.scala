package relaks.optimizer

import scala.collection.mutable
import scala.reflect.runtime.universe._
import scala.language.dynamics

/**
 * Created by Pietras on 23/04/15.
 */
trait NondetParams extends NondetParamTypes {
  type ParamsSpace = scala.collection.Map[String, NondetParamType[Any]]
  type Params = Map[String, Any]

  case class Nondet(name: String, value: NondetParamType[Any])

  case class ParamProvider(private val elems: (String, NondetParamType[Any])*) extends Dynamic {
    //an order of elements is required for spearmint
    val paramSpace: ParamsSpace = mutable.LinkedHashMap(elems:_*).toMap
    def selectDynamic(name: String): Nondet = Nondet(name, paramSpace(name))
  }

}