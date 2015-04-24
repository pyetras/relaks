package relaks.lang.dsl

import AST._
import AST.syntax._

import scala.collection.mutable

/**
 * Created by Pietras on 16/04/15.
 */
trait StdlibFunctions {
  var funCatalog = new mutable.HashMap[(Expression, Seq[TType]), Seq[Any] => Any]

  def putFunction(name: Expression, fun: Seq[Any] => Any, argtypes: TType*) = funCatalog += (((name, argtypes), fun))
  def getFunction(name: Expression, argtypes: TType*) = funCatalog((name, argtypes))
}
