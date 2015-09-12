package relaks.lang.dsl.extensions

import com.twitter.bijection.Injection
import org.kiama.attribution.Attribution
import relaks.lang.ast._
import relaks.lang.dsl.AST._
import relaks.lang.dsl.AST.syntax._
import relaks.lang.dsl._
import relaks.lang.dsl.extensions.ast._
import relaks.lang.dsl.utils.{UnliftType, TypedSymbols, TreePrettyPrintable}
import shapeless._
import shapeless.ops.nat.ToInt
import shapeless.ops.tuple.ToTraversable
import shapeless.ops.{hlist, tuple}

import scala.language.{higherKinds, existentials, implicitConversions}
import scalaz.Scalaz._
import scalaz._

/**
 * Created by Pietras on 17/05/15.
 */
trait TableExtensions extends TableIO with TableOps {
  type RowN[L <: HList] = Rep[Tup[L]]
  type Row[A] = Rep[Tup[A :: HNil]]
  type Row2[A, B] = Rep[Tup[A :: B :: HNil]]
  type Row3[A, B, C] = Rep[Tup[A :: B :: C :: HNil]]
}

trait TableUtils extends Symbols with Queries {

  object outputFields extends Attribution {
    private val findFields: Expression => Vector[(String, TType)] = attr {
      case _ /> Transform(_, _, _ /> Pure(_ /> row)) => TupleWithNames.unapplyWithTypes(row).get
      case _ /> Transform(_, _, Query(q)) => findFields(q)
      case _ /> OptimizerResultTable(argTuple) => TupleWithNames.unapplyWithTypes(argTuple).get
      case NextQuery(next) => findFields(next)
      case _ => Vector.empty
    }

    val apply: Expression => Vector[Field] = findFields andThen { namesTypes =>
      namesTypes.map { case (x, y) => Field(Symbol(x), y) }
    }
  }

  object TupleWithNames {
    import scalaz.Scalaz._
    import scalaz._
    def unapply(expr: Expression): Option[(Vector[Expression], Vector[String])] = expr match {
      case _/>(t: TupleConstructor) => Some((t.tuple, t.names))
      case _ => None
    }
    def unapplyWithTypes(row: Expression): Option[Vector[(String, TType)]] = unapply(row)
      .map(_.zipped.map { case (expr, name) => (name, expr.tpe)})

    private val toFields = { namesTypes: Vector[(String, TType)] =>
      namesTypes.map { case (x, y) => Field(Symbol(x), y) } }

    def unapplyFields = (unapplyWithTypes _) andThen toFields.lift
  }
}

trait IHateScala {
  type TableRep[H <: HList] = Rep[TypedTable[Tup[H]]]
  type OptimizerRep[H <: HList] = Rep[UnfinishedGenTable[Tup[H]]]

  implicit def unliftTRType[H <: HList, L <: HList, R <: HList]
  (implicit ev: UnliftType.Aux[L, Rep, R]) :
  UnliftType.Aux[TableRep[H] :: L, Rep, TypedTable[Tup[H]] :: R] =
    new UnliftType[TableRep[H] :: L, Rep] { type Out = TypedTable[Tup[H]] :: R }

  implicit def unliftORType[H <: HList, L <: HList, R <: HList]
  (implicit ev: UnliftType.Aux[L, Rep, R]) :
  UnliftType.Aux[OptimizerRep[H] :: L, Rep, UnfinishedGenTable[Tup[H]] :: R] =
    new UnliftType[OptimizerRep[H] :: L, Rep] { type Out = UnfinishedGenTable[Tup[H]] :: R }
}

trait TableOps extends Symbols with Queries with TypedSymbols with TableUtils with IHateScala with TupleExtensions with NativeFunExtensions {

  trait BuildComprehensionTyped[H <: HList, +In <: Rep[Table], Out[_ <: HList]] {
    def apply[T <: HList](e: Atom): Out[T]
  }

  implicit def buildTypedTable[H <: HList]: BuildComprehensionTyped[H,TableRep[H], TableRep] =
    new BuildComprehensionTyped[H, TableRep[H], TableRep] {
      override def apply[T <: HList](e: Atom): TableRep[T] = new Rep[TypedTable[Tup[T]]] {
        override val tree: Atom = e
      }
    }

  implicit def buildOptimizerTable[H <: HList]: BuildComprehensionTyped[H, OptimizerRep[H], OptimizerRep] =
    new BuildComprehensionTyped[H, OptimizerRep[H], OptimizerRep] {
      override def apply[T <: HList](e: Atom): OptimizerRep[T] = new Rep[UnfinishedGenTable[Tup[T]]] {
        override val tree: Atom = e
      }
    }

  trait BuildComprehension[+T <: Rep[Table], Out] {
    def apply(query: Atom): Out
  }

  private type AsTypedCmp[S <: HList] = BuildComprehension[Rep[TypedTable[Tup[S]]], Rep[TypedTable[Tup[S]]]]

  implicit def untypedAsComprehension: BuildComprehension[Rep[UntypedTable], Rep[UntypedTable]] =
    new BuildComprehension[Rep[UntypedTable], Rep[UntypedTable]] {
      override def apply(query: Atom): Rep[UntypedTable] = new Rep[UntypedTable] {
        override val tree: Atom = query
      }
    }

  implicit def optimizerAsComprehension[H <: HList]: BuildComprehension[Rep[UnfinOpt[H]], Rep[UnfinOpt[H]]] =
    new BuildComprehension[Rep[UnfinOpt[H]], Rep[UnfinOpt[H]]] {
      override def apply(query: Atom): Rep[UnfinOpt[H]] =
        new Rep[UnfinOpt[H]] {
          override val tree: Atom = query
        }
    }

  implicit def typedAsComprehension[H <: HList]: AsTypedCmp[H] =
    new BuildComprehension[Rep[TypedTable[Tup[H]]], Rep[TypedTable[Tup[H]]]] {
      override def apply(query: Atom): Rep[TypedTable[Tup[H]]] =
        new Rep[TypedTable[Tup[H]]] {
          override val tree: Atom = query
        }
    }

  class TypedTableOps[H <: HList, Out[_ <: HList]](arg: Rep[Table])
                                                  (implicit mkCmp: BuildComprehensionTyped[H, Rep[Table], Out])
    extends TupleGeneratorImpl {
    lazy val fields: Vector[Field] = outputFields.apply(arg.tree)

    private def flatMapImpl[A <: HList](f: Rep[Tup[A]] => Rep[_]) = {
      val (gen, tuple) = tupleGenerator(fields)
      val mapper = f(tuple)

      //TODO this could return a typed table
      val expr = Transform(gen, arg.tree, mapper.tree)
      expr
    }

    def flatMap[T <: HList](f: Rep[Tup[H]] => Rep[TypedTable[Tup[T]]]): Out[T] = {
      val expr = flatMapImpl(f)(new UntypedTableType) //TODO this could return a typed table
      mkCmp[T](expr)
    }

//    def flatMap(f: Rep[Tup[H]] => Rep[UntypedTable]): Rep[UntypedTable] = {
//      val expr = flatMapImpl(f)
//      new Rep[UntypedTable] {
//        override val tree: Atom = expr(new UntypedTableType)  //TODO this must be an atom for some reason (probably because of the lack of sym memoization)
//      }
//    }

    def map[F <: HList, T <: HList](f: Rep[Tup[H]] => Rep[Tup[T]]) = {
      flatMap(f andThen SingleRow.apply)
    }
  }

  implicit def addTypedTableOps[H <: HList](arg: Rep[TypedTable[Tup[H]]])
                                           (implicit mkCmp: BuildComprehensionTyped[H, TableRep[H], TableRep]):
  TypedTableOps[H, TableRep] =
    new TypedTableOps[H, TableRep](arg)

  implicit def addTypedUnfinishedOptimizerTableOps[H <: HList](arg: OptimizerRep[H])
                                                                   (implicit mkCmp: BuildComprehensionTyped[H, OptimizerRep[H], OptimizerRep]):
  TypedTableOps[H, OptimizerRep] =
    new TypedTableOps[H, OptimizerRep](arg)

  implicit class AvgTableOps[T](arg: Rep[TypedTable[Tup[T :: HNil]]])(implicit conv: Injection[T, Double]) {
    def avg: Rep[Double] = {
      val query = arg.map { case Tup(Tuple1(value: Rep[T])) => value.liftMap(x => conv(x)) as 'x0 }
      new Rep[Double] {
        val tree: Atom = Aggregate(Aggregator.Avg, query.tree)(implicitly[ArgType[Double]])
      }
    }
  }

  implicit class UntypedTableOps[Out[_ <: HList]](arg: Rep[UntypedTable])(implicit mkCmp: BuildComprehensionTyped[_, TableRep[_], Out]){
    def map[H <: HList](f: Rep[UntypedTup] => Rep[Tup[H]]): Out[H] = {
      val fnarg = freshRep[UntypedTup](UnknownType)
      val generator = EmptyGenerator(fnarg.tree.asInstanceOf[Sym])
      val transform = Transform(generator, arg.tree, SingleRow(f(fnarg)).tree)
      mkCmp(transform)
    }
  }

  implicit class LimitOps[In <: Rep[Table], Out](arg: In)(implicit asCmp: BuildComprehension[In, Out]) {
    def limit(count: Rep[Int]) =
      asCmp(Limit(arg.tree, Literal(0), count.tree))
    def limit(start: Rep[Int], count: Rep[Int]) =
      asCmp(Limit(arg.tree, start.tree, count.tree))
  }

  trait GetFieldsWithTypes {
    def getFieldsWithTypes(expr: Expression, fieldsVec: Vector[Symbol]) = {
      val mappedFields = outputFields.apply(expr).view.map(f => f.sym -> f).toMap //TODO this won't work on untyped
      fieldsVec.map(mappedFields)
    }
  }

  implicit class OrderByOps[In <: Rep[Table], Out](arg: In)(implicit asCmp: BuildComprehension[In, Out]) extends GetFieldsWithTypes {
    def orderBy[P <: Product](fields: P)(implicit toVector: ToTraversable.Aux[P, Vector, Symbol]): Out = {
      val fieldsVec = toVector(fields)
      val fieldsWithTypes = getFieldsWithTypes(arg.tree, fieldsVec)
      val expr = OrderBy(arg.tree, fieldsWithTypes.map(fld => FieldWithDirection(fld, OrderBy.Asc)))(new UntypedTableType)
      asCmp(expr)
    }

    def orderBy(field: Symbol): Out = orderBy(Tuple1(field))
  }

  trait TupleGeneratorImpl {
    protected def tupleGenerator[F <: HList](filterFields: Vector[Field]): (Generator, Rep[Tup[F]]) = {
      val gen = Generator.fromFields(filterFields)
      val tuple = gen.toTuple[F]
      (gen, tuple)
    }
  }

  abstract class ProjectionFilter[In <: Rep[Table], Out](arg: In)(implicit asCmp: BuildComprehension[In, Out]) extends TupleGeneratorImpl {
    def filter[P <: Product, F <: HList](fields: P)(f: Rep[Tup[F]] => Rep[Boolean])(implicit tupEv: IsTuple[P],
                                                                                    toVector: ToTraversable.Aux[P, Vector, Field]) = {
      val (generator, rep) = tupleGenerator(toVector(fields))
      val cond = f(rep).tree
      val filteredTable = Filter(generator, arg.tree, cond)
      asCmp(filteredTable)
    }
  }

  class ProjectionFilterComprehension[In <: Rep[UntypedTable], Out](arg: In)
                                              (implicit asCmp: BuildComprehension[In, Out])
    extends ProjectionFilter(arg)

  implicit def addUntypedFilter[Out](arg: Rep[UntypedTable])
                                    (implicit asCmp: BuildComprehension[Rep[UntypedTable], Out]):
    ProjectionFilterComprehension[Rep[UntypedTable], Out] = new ProjectionFilterComprehension(arg)

  class TypedFilterComprehension[H <: HList, In <: Rep[Table], Out[_ <: HList]](arg: In)
                                                                   (implicit mkCmp: BuildComprehension[In, Out[H]],
                                                                    addLazy: In => TypedTableOps[H, Out])
    extends ProjectionFilter(arg) {
    def filter(f: Rep[Tup[H]] => Rep[Boolean]) = {
      val (generator, rep) = tupleGenerator(arg.fields)
      val cond = f(rep).tree
      createFilterComprehension(generator, cond)
    }

    def withFilter(f: Rep[Tup[H]] => Rep[Boolean]): Out[H] = {
      val (generator, rep) = tupleGenerator(arg.fields)
      val cond = f(rep).tree
      cond match {
        case _ /> Literal(true) => mkCmp(arg.tree) //remove empty filters immediately, most likely occurs
                                                   // with the cast matcher _.isInstanceOf in for comprehensions
        case _ => createFilterComprehension(generator, cond)
      }
    }

    private def createFilterComprehension(generator: Generator, cond: Expression): Out[H] = {
      val filteredTable = Filter(generator, arg.tree, cond)
      mkCmp(filteredTable)
    }

  }

  implicit def addTypedFilter[H <: HList](arg: Rep[TypedTable[Tup[H]]]):
    TypedFilterComprehension[H, TableRep[H], TableRep] = new TypedFilterComprehension[H, TableRep[H], TableRep](arg)

  implicit def addGeneratorTypedFilter[H <: HList](arg: Rep[UnfinOpt[H]]):
    TypedFilterComprehension[H, OptimizerRep[H], OptimizerRep] = new TypedFilterComprehension[H, OptimizerRep[H], OptimizerRep](arg)

  implicit class ProjectionComprehension(arg: Rep[Table]) {
    def apply[P <: Product, FieldsLen <: Nat, L <: HList, S <: HList](schemaT: P)(implicit tupEv: IsTuple[P],
                                                                                 toHlist: Generic.Aux[P, L],
                                                                                 schema: TypedSymbolSchema.Aux[L, S],
                                                                                 typC: TupTypeConstructor[S],
                                                                                 lenEv: hlist.Length.Aux[S, FieldsLen],
                                                                                 schemaLength: ToInt[FieldsLen],
                                                                                  mkCmp: AsTypedCmp[S]): Rep[TypedTable[Tup[S]]] = {
      val fields = Tag.unwrap(schema(toHlist.to(schemaT)))
      val projection = Project.applyTyped[S](arg.tree, fields.map(f => f -> f.sym), typC(fields.map(_.typ)))
      mkCmp(projection)
    }

    def apply[E](field: TypedField[E])(implicit mkCmp: AsTypedCmp[E :: HNil]): Rep[TypedTable[Tup[E :: HNil]]] =
      apply(Tuple1(field))

  }

  private type UnfinOpt[H <: HList] = UnfinishedGenTable[Tup[H]]

  implicit class UnfinishedOptimizerOps[H <: HList](arg: Rep[UnfinOpt[H]])
                                                   (implicit mkCmp: AsTypedCmp[H])
    extends GetFieldsWithTypes {
        def by(field: Symbol) =
         mkCmp(OptimizeBy(arg.tree,
           Vector(FieldWithDirection(getFieldsWithTypes(arg.tree, Vector(field)).head, OrderBy.Asc))
         )(new UntypedTableType))

  }

//  class UntypedOptimizerComprehensions(override val tree: Expression) extends Rep[UntypedTable] { self =>
//    def by(field: Symbol): Rep[UntypedTable] = new Rep[UntypedTable] {
//      override val tree: Atom = OrderBy(self.tree, Vector(FieldWithDirection(field, GroupBy.Asc)))(new UntypedTableType)
//    }
//  }

  //IntelliJ not working with magnet
//  trait ProjectionMagnet[P <: Product] {
//    type Out
//    def apply(p: P, arg1: Rep[UntypedTable]): Out
//  }
//
//  object ProjectionMagnet {
//    type Aux[P <: Product, R] = ProjectionMagnet[P] { type Out = R }
//    def apply[P <: Product](implicit factory: ProjectionMagnet[P]): Aux[P, factory.Out] = factory
//
//    implicit def typedProjection[P <: Product, FieldsLen <: Nat, L <: HList, S <: HList](implicit tupEv: IsTuple[P],
//                                                                                         toHlist: Generic.Aux[P, L],
//                                                                                         schema: TypedSymbolSchema.Aux[L, S],
//                                                                                         lenEv: hlist.Length.Aux[S, FieldsLen],
//                                                                                         fieldsLength: ToInt[FieldsLen]):
//    Aux[P, ProjectedTypedTableComprehensions[S]] =
//      new ProjectionMagnet[P] {
//        override type Out = ProjectedTypedTableComprehensions[S]
//        override def apply(p: P, arg1: Rep[UntypedTable]): Out = new ProjectedTypedTableComprehensions[S](Tag.unwrap(schema(toHlist.to(p))), arg1.tree)
//      }
//
//    implicit def untypedProjection[P <: Product, FieldsLen <: Nat](implicit tupEv: IsTuple[P],
//                                                                              lenEv: tuple.Length.Aux[P, FieldsLen],
//                                                                              fieldsLength: ToInt[FieldsLen],
//                                                                              toVector: ToTraversable.Aux[P, Vector, Symbol]): Aux[P, ProjectedTableComprehensions[FieldsLen]] =
//
//      new ProjectionMagnet[P] {
//        override type Out = ProjectedTableComprehensions[FieldsLen]
//        override def apply(p: P, arg1: Rep[UntypedTable]): Out = new ProjectedTableComprehensions[FieldsLen](toVector(p), arg1.tree)
//      }
//  }

}

trait TableIO extends Symbols {
  private[lang] var storedOutput: Set[Expression] = Set.empty

  def load(path: String): Rep[UntypedTable] = new Rep[UntypedTable] {
    override val tree: Atom = LoadTableFromFs(path)(new UntypedTableType)
  }

  def load[S <: HList](path: String, schema: Rep[Tup[S]]): Rep[TypedTable[S]] = new Rep[TypedTable[S]] {
    override val tree: Atom = LoadTableFromFs(path)(schema.getTpe)
  }

  def store(table: Rep[Table]): Unit = storedOutput += table.tree
}
