import fwb.dsl.flow.Flow
import Flow._
import fwb.backends.tester.Algorithms.{train, Model}
import shapeless._
import shapeless.ops.function.FnToProduct
import shapeless.ops.record.Selector
import syntax.singleton._
import scalax.collection.Graph
import scalax.collection.edge.LDiEdge
import scalax.collection.edge.Implicits._
import record._
val z = Witness('model)
val x = Coproduct[String :+: Int :+: CNil]("svm")
val params = ('model ->> x) :: HNil
val params2 = params + ('model ->> Coproduct[String :+: Int :+: CNil](1))
params2.get(z).select[String]
params2.keys

//typeOf[train]
val n = Node[train]
val n2 = Node[train]
val edges = List((n ~+> n2)("hello", "world"), (n2 ~+> n)("hell2o", "world2"))
val g = Graph.from(List(), edges)
val vertex = g get n
vertex.diSuccessors

val labG = LabelledGeneric[train]
val rec = labG.to(train(List(true)))
rec.get('target)
val rec2 = ('target ->> List(true)) :: HNil
//shapeless.::[List[Boolean] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("target")],List[Boolean]],shapeless.HNil]
//shapeless.::[List[Boolean] with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("target")],List[Boolean]],shapeless.HNil]

val k = Witness('target)
case class Test(w: Witness)
val l = Witness('ret)
def getk[L <: HList](key: Witness)(implicit sel: Selector[L, key.T]) = sel
getk[labG.Repr](k)
val edge = n('ret) ~> n2(k)
edge.label
def f10[R <: HList, W1](el: Node[R, _], l: EdgeLabel[_, W1])(implicit sel: Selector[R, W1]) = sel
val sel = f10(n, edge.label).apply(rec)

val gr = Graph.from(edges = List(edge))
(gr get n).edges.head.label
val sel2 = f10(n, (gr get n).edges.head.label).apply(rec)
