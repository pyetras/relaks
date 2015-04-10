package fwb.dsl

import scala.language.experimental.macros

/**
 * Created by Pietras on 31/03/15.
 */
trait CallApply[C] {
  type Ret
  def apply(c: C): Ret
}

object CallApply {
  type Aux[C, R] = CallApply[C] { type Ret = R }

  implicit def materialize[C, R]: Aux[C, R] = macro CallApplyImpl.materialize[C]
}

object CallApplyImpl {
  import scala.reflect.macros.whitebox

  def materialize[C: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    val C = weakTypeOf[C]
    val assignM = C.decls.collect {
      case sym: MethodSymbol if sym.name == TermName("apply") => sym
    }
    if (assignM.headOption.isEmpty) c.abort(c.enclosingPosition, "case class must define an apply() method")

    val R = assignM.head.returnType

    q"""new _root_.fwb.dsl.CallApply[$C] { type Ret = $R; def apply(c: $C) : $R = c.apply() }""" // TODO: make this parameterless
  }
}
