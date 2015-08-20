package relaks.lang.dsl.extensions

import com.typesafe.scalalogging.LazyLogging
import relaks.data.LabelledTuples
import relaks.lang.ast._
import relaks.lang.dsl.AST._
import relaks.lang.dsl._
import relaks.lang.dsl.extensions.ast.Symbols
import relaks.lang.dsl.utils.{NamesToStrings, FillNat, TupleLU, UnliftType}
import relaks.lang.impl.Row
import relaks.lang.phases.interpreter.BaseExprInterpreter
import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.hlist._
import shapeless.ops.record
import shapeless.ops.nat.ToInt
import shapeless.syntax.std.tuple._

import scala.language.{existentials, higherKinds, implicitConversions, reflectiveCalls}
import scalaz.Tag


/**
 * Created by Pietras on 16/04/15.
 */

trait TupleExtensions extends Symbols with AnyExtensions with LazyLogging with LabelledTuples {

  object Tup {

    /**
     * Functor for mapping HList of type (Nat, Rep[Tup[_] ]) to a proper Rep[_]
      */
    private object toReps extends Poly1 {
      implicit def f[N <: Nat, T<:HList](implicit att: At[T, N], toInt: ToInt[N]) =
        at[(N, Rep[Tup[T]])](t => t._2.extract[att.Out](toInt()))
    }


    /**
     * @param r tuple tree node
     *
     * @tparam T type of the tuple representation
     * @tparam N length of the tuple - 1
     * @tparam Seq seq of nats from 0 to N
     * @tparam Zipped reversed Seq zipped with r
     * @tparam Reversed seq reversed
     * @tparam Mapped hlist mapped to Rep[_] values
     * @return
     */
    def unapply[T <: HList, N <: Nat, Seq <: HList, Zipped <: HList, Reversed <: HList, Mapped <: HList](r: Rep[Tup[T]])(implicit
                                                                      len: Length.Aux[T, Succ[N]],
                                                                      seq: FillNat.Aux[N, Seq],
                                                                      rev: Reverse.Aux[Seq, Reversed],
                                                                      zip: ZipConst.Aux[Rep[Tup[T]], Reversed, Zipped],
                                                                      mapper: Mapper.Aux[toReps.type, Zipped, Mapped],
                                                                      tupler: Tupler[Mapped]) = r.tree match {
      case TupleConstructor(lst) =>
        Some(seq().reverse.zipConst(r).map(toReps).tupled)
      case _ => None
    }
  }

  class TupleOperations[B1 <: HList](val arg1: Rep[Tup[B1]]) {
    lazy val (luType, productTypes) = arg1.getTpe match {
      case t: TupType[B1] => (t.lowerBound, t.childrenTypes)
      case _ => (UnknownType, new Seq[TType]{
        override def length: Int = ???
        override def apply(idx: Int): TType = UnknownType
        override def iterator: Iterator[TType] = ???
      })
    }

    /**
     * Static (compile time) getter
     */
    def apply[T](i: Nat)(implicit toInt: ToInt[i.N], at: At.Aux[B1, i.N, T]) : Rep[T] = extract[T](toInt())

    /**
     * Dynamic (runtime) getter
     */
    def at[LUB](i: Rep[Int])(implicit ev: TupleLU[B1, LUB]): Rep[LUB] = new Rep[LUB] { //FIXME jakos?
      override val tree: Expression = Apply(Stdlib.at, List(arg1.tree, i.tree))(luType)
    }

    private[dsl] def extract[Out](i: Int): Rep[Out] = {
      def access(i: Rep[Int], tpe: TType) = new Rep[Out] {
        override val tree: Expression = Apply(Stdlib.at, List(arg1.tree, i.tree))(UnknownType)
      }

      arg1.tree match {
        case Expr(TupleConstructor(seq)) => new Rep[Out] {
          override val tree: Expression = seq(i)
        }
        case (e: TTree) :@ (t: TType) => {//in case it doesn't have an actual value assigned (fresh for example)
          logger.warn("Static access to an uninitialized tuple")
          access(i, productTypes(i))
        }
      }
    }
  }

  implicit def tupleToTupleOps[H <: HList](p: Rep[Tup[H]]): TupleOperations[H] = new TupleOperations[H](p)

  /**
   * this will lift any type T to a Rep[T]
    */
  private object asRep extends Poly1 {
    implicit def lifted[T <: Rep[_]] = at[T](x => x)
    implicit def unlifted[T: ArgType](implicit c: T => Rep[T]) = at[T](x => c(x))
  }

  trait HListToNode[L <: HList] {
    type Out <: HList
    def apply(l: L): (Vector[Expression], TType)
  }

  object HListToNode {
    type Aux[L <: HList, R <: HList] = HListToNode[L] { type Out = R }
    def apply[L <: HList](implicit htn: HListToNode[L]): Aux[L, htn.Out] = htn

    implicit def fromHlist[H <: HList, R <: HList, LU, Mapped <: HList](implicit ul: UnliftType.Aux[H, Rep, R],
                                                                        evlu: TupleLU[R, LU],
                                                                        typC: TupTypeConstructor[R],
                                                                        mapper: Mapper.Aux[asRep.type, H, Mapped],
                                                                        traversable: ToTraversable.Aux[Mapped, List, Rep[_]]): Aux[H, R] =
      new HListToNode[H] {
        override type Out = R
        override def apply(hlist: H): (Vector[Expression], TType) = {
          val replist = hlist.map(asRep).toList[Rep[_]] // <: List[Rep[Any]]
          val typ = typC(replist.map(_.getTpe).toVector)
          (replist.map(_.tree).toVector, typ)
        }
      }
  }

  def tupleToRep2[P <: Product, H <: HList, R <: HList, LU, Mapped <: HList](p: P)
                                                               (implicit ev: Generic.Aux[P, H],
                                                                tev: IsTuple[P], ul: UnliftType.Aux[H, Rep, R],
                                                                evlu: TupleLU[R, LU],
                                                                typC: TupTypeConstructor[R],
                                                                mapper: Mapper.Aux[asRep.type, H, Mapped],
                                                                traversable: ToTraversable.Aux[Mapped, List, Rep[_]]) = null


  implicit def tupleToRep[P <: Product, H <: HList, R <: HList](p: P)
                                                               (implicit ev: Generic.Aux[P, H],
                                                                tev: IsTuple[P], toNode: HListToNode.Aux[H, R]): Rep[Tup[R]] = {
    val hlist = ev.to(p)
    val (exprs, typ) = toNode(hlist)
    new Rep[Tup[R]] {
      override val tree: Expression = TupleConstructor(exprs)(typ)
    }
  }

  trait LabelledTupleToRep[R <: HList] {
    type Out <: HList
    def apply(p: LabelledTuple[R]): Rep[Tup[Out]]
  }

  object LabelledTupleToRep {
    type Aux[R <: HList, RR <: HList] = LabelledTupleToRep[R] {type Out = RR}

    def apply[R <: HList](implicit lttr: LabelledTupleToRep[R]): Aux[R, lttr.Out] = lttr

    implicit def labelledTupleToRep[R <: HList, H <: HList, ROut <: HList, Names <: HList](
      implicit values: ops.record.Values.Aux[R, H],
      names: ops.record.Keys.Aux[R, Names],
      namesT: NamesToStrings[Names],
      toNode: HListToNode.Aux[H, ROut]
      ): Aux[R, ROut] = new LabelledTupleToRep[R] {
      override type Out = ROut

      override def apply(p: LabelledTuple[R]): Rep[Tup[Out]] = {
        val hlist = values.apply(p.asHList)
        val (exprs, typ) = toNode(hlist)
        new Rep[Tup[Out]] {
          override val tree: Expression = TupleConstructor(exprs, namesT(names()))(typ)
        }
      }
    }
  }

  implicit def labelledTupleToRep[R <: HList, Out <: HList](p: LabelledTuple[R])
                                             (implicit lttr: LabelledTupleToRep.Aux[R, Out]): Rep[Tup[Out]] =
    lttr(p)

  implicit def tupleWithLabelsToRep[P <: Product, L <: HList, R <: HList, Out <: HList](tup: P)(implicit ev1: IsTuple[P],
                                                                     toHlist: Generic.Aux[P, L],
                                                                     evR: Mapper[isRecord.type, L],
                                                                     lttr: LabelledTupleToRep.Aux[L, Out]): Rep[Tup[Out]] =
    lttr(new LabelledTuple(toHlist.to(tup)))

  implicit def singleFieldToRep[K, V, Out <: HList](field: FieldType[K, V])
                                                   (implicit lttr: LabelledTupleToRep.Aux[FieldType[K, V] :: HNil, Out]): Rep[Tup[Out]] =
    lttr(new LabelledTuple(field :: HNil))

  trait AsTuple[T] {
    type Out <: HList
    def apply(r: Rep[T]): Rep[Tup[Out]]
  }

  object AsTuple {
    type Aux[T, O <: HList] = AsTuple[T] { type Out = O }
    def apply[T](implicit asTuple: AsTuple[T]): Aux[T, asTuple.Out] = asTuple
    implicit def repToTuple[T](implicit ev: T =:!= Tup[_]): Aux[T, T :: HNil] = new AsTuple[T] {
      override type Out = T :: HNil
      override def apply(r: Rep[T]): Rep[Tup[Out]] = new Rep[Tup[T :: HNil]] {
        override val tree: TTree = TupleConstructor(Vector(r.tree))
      }
    }

    implicit def tupleNoOp[H <: HList]: Aux[Tup[H], H] = new AsTuple[Tup[H]] {
      override type Out = H
      override def apply(r: Rep[Tup[H]]): Rep[Tup[H]] = r
    }
  }


}

trait TupleInterpreter extends BaseExprInterpreter with Symbols {
  def evalTupleExpression: PartialFunction[Expression, Row] = {
    case _/>(tup: TupleConstructor) =>
      new Row(tup.tuple.map(evalExpression), tup.names zip tup.tpe.asInstanceOf[TupType[_]].childrenTypes)
  }

  override def evalExpression(expr: Expression): Any = evalTupleExpression.applyOrElse(expr, super.evalExpression)
}

