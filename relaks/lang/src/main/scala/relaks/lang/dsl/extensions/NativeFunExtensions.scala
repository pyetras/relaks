package relaks.lang.dsl.extensions

import relaks.lang.ast.{ArgType, ApplyNative, Expression, Tup}
import relaks.lang.dsl.AST.ASTSyntax
import relaks.lang.dsl.Rep
import shapeless.HList
import shapeless.ops.function.FnToProduct
import shapeless.syntax.std.function._
/**
 * Created by Pietras on 23/06/15.
 */
trait NativeFunExtensions extends ASTSyntax with AnyExtensions {
  class CallWord[H <: HList, T](f: H => Rep[T], typ: ArgType[T]) {
    def apply(args: Rep[Tup[H]]): Rep[T] = new Rep[T] {
      override val tree: Expression = ApplyNative(f, args.tree)(typ)
    }
  }

  def to[F, H <: HList, R, T](f: F)(implicit fnToProduct: FnToProduct.Aux[F, H => R], represent: R => Rep[T] = null, typ: ArgType[T]) =
    new CallWord(f.toProduct andThen (if (represent != null) represent else (x => x.asRep)), typ)
}
