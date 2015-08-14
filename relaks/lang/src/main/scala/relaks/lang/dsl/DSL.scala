package relaks.lang.dsl

import relaks.lang.dsl.AST.ASTSyntax
import relaks.lang.dsl.extensions._
import relaks.lang.dsl.extensions.ast.Symbols
import relaks.lang.phases.interpreter.{StdOpInterpreter, NativeInterpreter, QueryOpInterpreter, OptimizationInterpreter}
import relaks.optimizer.{NondetParams, NondetParam, BaseOptimizer, GridOptimizer}

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
  with TableIO


abstract class DSLInterpreter(optimizer: BaseOptimizer = GridOptimizer) extends OptimizationInterpreter(optimizer)
  with DSL
  with QueryOpInterpreter
  with NativeInterpreter
  with StdOpInterpreter
