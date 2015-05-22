package relaks.lang.dsl.extensions

import com.typesafe.scalalogging.LazyLogging
import relaks.lang.dsl._
import AST._
import relaks.lang.dsl.utils.{FillNat, UnliftType}
import shapeless._
import shapeless.ops.traversable.FromTraversable
import shapeless.ops.traversable.FromTraversable._
import shapeless.ops.hlist._
import shapeless.ops.nat.ToInt
import shapeless.syntax.std.tuple._
import shapeless.syntax.std.traversable._
import relaks.lang.dsl.AST.syntax._


import scala.language.implicitConversions
import scala.language.higherKinds
import scala.language.existentials
import scala.language.reflectiveCalls


/**
 * Created by Pietras on 16/04/15.
 */

trait TupleExtensions extends Symbols with AnyExtensions with LazyLogging {

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
      case t: TupType[B1] => (t.lowerBound, t.productTypes)
      case _ => (UnknownType, new Seq[TType]{
        override def length: Int = ???
        override def apply(idx: Int): AST.TType = UnknownType
        override def iterator: Iterator[AST.TType] = ???
      })
    }

    /**
     * Static (compile time) getter
     */
    def apply(i: Nat)(implicit toInt: ToInt[i.N], at: At[B1, i.N]) : Rep[at.Out] = extract[at.Out](toInt())

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
    implicit def lifted[T: ArgType] = at[Rep[T]](x => x)
    implicit def unlifted[T: ArgType](implicit c: T => Rep[T]) = at[T](x => c(x))
  }

  implicit def tupleToRep[P <: Product, H <: HList, R <: HList, LU, Mapped <: HList](p: P)
                                                               (implicit ev: Generic.Aux[P, H],
                                                                ul: UnliftType.Aux[H, Rep, R],
                                                                evlu: TupleLU[R, LU],
                                                                typC: TupTypeConstructor[R],
                                                                tev: IsTuple[P],
                                                                mapper: Mapper.Aux[asRep.type, H, Mapped],
                                                                traversable: ToTraversable.Aux[Mapped, List, Rep[LU]]): Rep[Tup[R]] = {
    val hlist = ev.to(p)
    val replist = hlist.map(asRep).toList[Rep[LU]] // <: List[Rep[Any]]
    val typ = typC(replist.map(_.getTpe).toVector)
    new Rep[Tup[R]] {
      override val tree: Expression = TupleConstructor(replist.map(_.tree).toVector)(typ)
    }
  }
}

