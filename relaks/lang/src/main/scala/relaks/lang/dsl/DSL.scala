package relaks.lang.dsl

import relaks.lang.dsl.AST.ASTSyntax
import relaks.lang.dsl.extensions._

import scala.collection.mutable
import scala.language.implicitConversions

/**
 * Created by Pietras on 14/04/15.
 */
trait DSL
  extends ASTSyntax
  with Symbols
  with SuperPosExtensions
  with AnyExtensions
  with BoolExtensions
  with OrderExtensions
  with NumericExtensions
  with ListExtensions
  with TupleExtensions
  with NativeFunExtensions
  with TableExtensions
  with ContCompiler {

}

trait DSLInterpreter extends Symbols
with SuperPosExtensions
with AnyExtensions
with BoolExtensions
with OrderExtensions
with NumericExtensions
with ListExtensions
with TupleExtensions
with NativeFunExtensions
with TableExtensions
with Interpreter {

}
