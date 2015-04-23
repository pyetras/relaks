package relaks.optimizer

import scala.reflect.runtime.universe._
import scala.language.dynamics

/**
 * Created by Pietras on 23/04/15.
 */
trait NondetVars extends VarSpaceTypes {
  type VarSpaceDesc = Map[String, VarSpace[Any]]
  type ValStore = Map[String, Any]

  case class Nondet(name: String, value: VarSpace[Any])

  case class VarProvider(vars: VarSpaceDesc) extends Dynamic {
    def selectDynamic(name: String): Nondet = Nondet(name, vars(name))
  }

}