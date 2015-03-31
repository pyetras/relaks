import fwb.api.Flow.Node
import fwb.backends.tester.Algorithms.{train, Model}
import shapeless._
import shapeless.ops.function.FnToProduct
import syntax.singleton._
import scala.reflect.runtime.universe._
import record._
val z = Witness('model)
val x = Coproduct[String :+: Int :+: CNil]("svm")
val params = ('model ->> x) :: HNil
val params2 = params + ('model ->> Coproduct[String :+: Int :+: CNil](1))
params2.get(z).select[String]
params2.keys

//typeOf[train]
val n = Node[]

//train
//n.call(('target ->> List(true)) :: HNil)
def f = identity[Int] _
val n2 = Node(f)
//val f2 = (_:Int) + (_:Int)
//val fp = implicitly[FnToProduct.Aux[(Int, Int) => Int, Int ::Int::HNil => Int]]
val i = n2.call(10 :: HNil)

//import scala.language.experimental.macros
//implicit def applyRetType[C, R]: ApplyRetType[C, R] = macro ApplyRetTypeImpl.materialize[C, R]
//def f4[C, R](implicit ev: ApplyRetType[C, R]) = ev
//f4[train]
//val t = train
//weakTypeOf[train.type].companion

////val apl = lens[Model] >> 'apply
//
//def labs[L, K <: HList](labels: L)(implicit lab: DefaultSymbolicLabelling.Aux[L, K]) = {
//  labels
//}
//labs(f2)

def f5[L](f: L)(implicit fnToProduct: FnToProduct[L]) = fnToProduct
f5(train.apply _)

