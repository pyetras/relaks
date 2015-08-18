package relaks.lang.dsl.extensions

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.core.JsonParser.Feature
import com.fasterxml.jackson.databind.{SerializationFeature, ObjectMapper}
import com.fasterxml.jackson.databind.module.SimpleModule
import com.typesafe.scalalogging.LazyLogging
import org.apache.calcite.rel.core.JoinRelType
import org.kiama.attribution.Attribution
import org.kiama.relation.GraphTree
import org.kiama.==>
import org.kiama.rewriting.{Rewriter, Strategy}
import relaks.lang.ast._
import relaks.lang.dsl.AST._
import relaks.lang.dsl.AST.syntax._
import relaks.lang.dsl._
import relaks.lang.dsl.extensions.ast._
import relaks.lang.dsl.extensions.ast.logical.{QueryOp, LoadComprehension, SelectComprehension}
import relaks.lang.dsl.utils.{TypedSymbols, TreePrettyPrintable}
import relaks.lang.impl.Row
import relaks.lang.phases.interpreter.{BaseQueryOpInterpreter, BaseExprInterpreter}
import shapeless._
import shapeless.ops.nat.ToInt
import shapeless.ops.tuple.ToTraversable
import shapeless.ops.{hlist, tuple}

import scala.collection.immutable.Stack
import scala.collection.mutable
import scala.language.{existentials, implicitConversions}
import scalaz.Scalaz._
import scalaz._

/**
 * Created by Pietras on 17/05/15.
 */
trait TableExtensions extends TableIO with TableOps with TableUtils {

}

trait TableUtils extends Symbols with Queries {

  object outputNamesTypes extends Attribution {
    val apply: Query => Vector[(String, TType)] = attr {
      case _ /> Transform(_, _, _ /> Pure(_ /> row)) => TupleWithNames.unapplyWithTypes(row).get
      case _ /> Transform(_, _, Query(q)) => apply(q)
      case NextQuery(next) => apply(next)
    }
  }

}

trait TableOps extends Symbols with Queries with TypedSymbols with TableUtils {

  type RowN[L <: HList] = Rep[Tup[L]]
  type Row[A] = Rep[Tup[A :: HNil]]
  type Row2[A, B] = Rep[Tup[A :: B :: HNil]]
  type Row3[A, B, C] = Rep[Tup[A :: B :: C :: HNil]]


  class ProjectedTableComprehensions[FieldsLen <: Nat](private[extensions] val fields: Vector[Field],
                                                       private[extensions] val query: Atom)
    extends Rep[UntypedTable] {

    override val tree: Expression = query

    private def tupleGenerator[F <: HList](filterFields: Vector[Field] = fields): (Generator, Rep[Tup[F]]) = {
      val gen = Generator.fromFields(filterFields)
      val tuple = gen.toTuple[F] //todo types
      (gen, tuple)
    }
    private def flatMapImpl[A <: HList](f: Rep[Tup[A]] => Rep[_]) = {
      val (gen, tuple) = tupleGenerator()
      val mapper = f(tuple)

      //TODO this could return a typed table
      val expr = Transform(gen, query, mapper.tree)
      expr
    }

    def flatMap[F <: HList](f: Rep[Tup[F]] => Rep[UntypedTable])(implicit lenEv: hlist.Length.Aux[F, FieldsLen]): Rep[UntypedTable] = {
      val expr = flatMapImpl(f)
      new Rep[UntypedTable] {
        override val tree: Atom = expr(new UntypedTableType)  //TODO this must be an atom for some reason (probably because of the lack of sym memoization)
      }
    }

    def flatMap[F <: HList, T <: HList](f: Rep[Tup[F]] => Rep[TypedTable[Tup[T]]])(implicit lenEv: hlist.Length.Aux[F, FieldsLen], lenOutEv: hlist.Length[T]) = {
      val expr = flatMapImpl(f)
      val names = outputNamesTypes.apply(expr)
      new ProjectedTypedTableComprehensions[T](names.map { case (x, y) => Field(Symbol(x), y) }, expr(new UntypedTableType)) //TODO this could return a typed table
    }

    def map[F <: HList, T <: HList](f: Rep[Tup[F]] => Rep[Tup[T]])(implicit lenEv: hlist.Length.Aux[F, FieldsLen], lenOutEv: hlist.Length[T]) = {
      flatMap(f andThen RowRep.apply)
    }

    def withFilter(f: Rep[Tup[Nothing]] => Rep[Boolean]) = {
      val (generator, rep) = tupleGenerator()
      val cond = f(rep).tree
      cond match {
        case _/>Literal(true) => this //remove empty filters immediately, most likely occurs
                                   // with the cast matcher _.isInstanceOf in for comprehensions
        case _ => createFilterComprehension(generator, cond)
      }
    }

    private def createFilterComprehension(generator: Generator, cond: Expression): ProjectedTableComprehensions[FieldsLen] = {
      val filteredTable = Filter(generator, query, cond)
      new ProjectedTableComprehensions[FieldsLen](fields, filteredTable)
    }

//    def filter[F <: HList](f: Rep[Tup[F]] => Rep[Boolean])(implicit lenEv: hlist.Length.Aux[F, FieldsLen]) = {
//      val (generator, rep) = tupleGenerator(fields)
//      val cond = f(rep).tree
//      createFilterComprehension(generator, cond)
//    }
  }

  class ProjectedTypedTableComprehensions[H <: HList](private[extensions] val fields: Vector[Field], private[extensions] val query: Atom)
                                                                (implicit val lenEnv: hlist.Length[H])
    extends Rep[TypedTable[Tup[H]]] {

    override val tree: TTree = query

    implicit val lenEnvAux = new hlist.Length[H] {
      override type Out = lenEnv.Out
      override def apply(): lenEnv.Out = lenEnv.apply()
    }

    lazy val untypedComprehension = new ProjectedTableComprehensions[lenEnv.Out](fields, query)

    def flatMap(f: Rep[Tup[H]] => Rep[UntypedTable]) = untypedComprehension.flatMap(f)
    def flatMap[T <: HList](f: Rep[Tup[H]] => Rep[TypedTable[Tup[T]]])(implicit lenEnv: hlist.Length[T]) = untypedComprehension.flatMap(f)
    def map[T <: HList](f: Rep[Tup[H]] => Rep[Tup[T]])(implicit lenEnv: hlist.Length[T]) = untypedComprehension.map(f)
    val withFilter = (untypedComprehension.withFilter _) andThen fromUntypedTableComprehension //TODO this does not enforce types

    private def fromUntypedTableComprehension(comprehensions: ProjectedTableComprehensions[lenEnv.Out]) =
      new ProjectedTypedTableComprehensions[H](comprehensions.fields, comprehensions.query)
  }

  abstract class AsComprehension[+T <: Rep[Table], Out](val arg: T, val fields: Vector[Field]) {
    def create(fields: Vector[Field], query: Atom): Out
  }

  class LimitComprehension[Out](arg: AsComprehension[Rep[Table], Out]) {
    def limit(count: Rep[Int]) =
      arg.create(arg.fields, Limit(arg.arg.tree, Literal(0), count.tree))
    def limit(start: Rep[Int], count: Rep[Int]) =
      arg.create(arg.fields, Limit(arg.arg.tree, start.tree, count.tree))
  }

  implicit def limitComprehension[T <: Rep[Table], X](arg: T)(implicit cmp: T => AsComprehension[Rep[Table], X]) = new LimitComprehension(arg)

  class OrderByComprehension[Out](arg: AsComprehension[Rep[Table], Out]) {
    def orderBy[P <: Product](fields: P)(implicit toVector: ToTraversable.Aux[P, Vector, Symbol]): Out = {
      val fieldsVec = toVector(fields)
      val expr = OrderBy(arg.arg.tree, fieldsVec.map(FieldWithDirection(_, GroupBy.Asc)), isExperimentTarget = true)(new UntypedTableType)
      arg.create(arg.fields, expr)
    }

    def orderBy(field: Symbol): Out = orderBy(Tuple1(field))
  }

  implicit def orderByComprehension[T <: Rep[Table], X](arg: T)(implicit cmp: T => AsComprehension[Rep[Table], X]) = new OrderByComprehension(arg)

  trait TupleGenerator {
    protected def tupleGenerator[F <: HList](filterFields: Vector[Field]): (Generator, Rep[Tup[F]]) = {
      val gen = Generator.fromFields(filterFields)
      val tuple = gen.toTuple[F]
      (gen, tuple)
    }
  }

  abstract class ProjectionFilter[In <: Rep[Table], Out](arg: AsComprehension[In, Out]) extends TupleGenerator {
    def filter[P <: Product, FL <: Nat, F <: HList](fields: P)(f: Rep[Tup[F]] => Rep[Boolean])(implicit
                                                                                                 lenEv: tuple.Length.Aux[P, FL],
                                                                                               lenEnv2: hlist.Length.Aux[F, FL],
                                                                                               toVector: ToTraversable.Aux[P, Vector, Field]) = {
      val (generator, rep) = tupleGenerator(toVector(fields))
      val cond = f(rep).tree
      val filteredTable = Filter(generator, arg.arg.tree, cond)
      arg.create(arg.fields, filteredTable)
    }
  }

  class ProjectionFilterComprehension[Out](arg: AsComprehension[Rep[UntypedTable], Out]) extends ProjectionFilter(arg)

  implicit def projectionFilterComprehension[T <: Rep[UntypedTable], X](arg: T)
                                                                       (implicit cmp: T => AsComprehension[Rep[UntypedTable], X]) =
    new ProjectionFilterComprehension(arg)

  class TypedFilterComprehension[H <: HList, Out](arg: AsComprehension[Rep[TypedTable[Tup[H]]], Out]) extends ProjectionFilter(arg) {
    def filter(f: Rep[Tup[H]] => Rep[Boolean]) = {
      val (generator, rep) = tupleGenerator(arg.fields)
      val cond = f(rep).tree
      val filteredTable = Filter(generator, arg.arg.tree, cond)
      arg.create(arg.fields, filteredTable)
    }
  }

  type MkAsCmp[-A, +B <: Rep[Table], C] = A => AsComprehension[B, C]

  implicit def typedFilterComprehension1[X, H <: HList](arg: TypedOptimizerComprehensions[H])
                                                       (implicit cmp: MkAsCmp[TypedOptimizerComprehensions[H], Rep[TypedTable[Tup[H]]], X]) =
    new TypedFilterComprehension(arg)

  implicit def typedFilterComprehension2[X, H <: HList](arg: ProjectedTypedTableComprehensions[H])
                                                       (implicit cmp: MkAsCmp[ProjectedTypedTableComprehensions[H], Rep[TypedTable[Tup[H]]], X]) =
    new TypedFilterComprehension(arg)

  implicit class ProjectionComprehension(arg: Rep[Table]) {
    def apply[P <: Product, FieldsLen <: Nat, L <: HList, S <: HList](schemaT: P)(implicit tupEv: IsTuple[P],
                                                                                 toHlist: Generic.Aux[P, L],
                                                                                 schema: TypedSymbolSchema.Aux[L, S],
                                                                                  typC: TupTypeConstructor[S],
                                                                                 lenEv: hlist.Length.Aux[S, FieldsLen],
                                                                                 schemaLength: ToInt[FieldsLen]) = {
      val fields = Tag.unwrap(schema(toHlist.to(schemaT)))
      val projection = Project.applyTyped[S](arg.tree, fields.map(f => f -> f.sym), typC(fields.map(_.typ)))
      new ProjectedTypedTableComprehensions[S](fields, projection)
    }

    def apply[E](field: TypedField[E]): ProjectedTypedTableComprehensions[E :: HNil] =
      apply(Tuple1(field))

  }

  implicit def untypedAsComprehension(arg: Rep[UntypedTable]): AsComprehension[Rep[UntypedTable], Rep[UntypedTable]] =
    new AsComprehension[Rep[UntypedTable], Rep[UntypedTable]](arg, null) {
    override def create(fields: Vector[Field], query: Atom): Rep[UntypedTable] = new Rep[UntypedTable] {
      override val tree: TTree = query
    }
  }

  implicit def typedOptimizerAsComprehension[H <: HList](arg: TypedOptimizerComprehensions[H]):
  AsComprehension[TypedOptimizerComprehensions[H], TypedOptimizerComprehensions[H]] =
    new AsComprehension[TypedOptimizerComprehensions[H], TypedOptimizerComprehensions[H]](arg, arg.fields) {
      implicit val lenEnv = arg.lenEnv
      override def create(fields: Vector[Field], query: Atom): TypedOptimizerComprehensions[H] =
        new TypedOptimizerComprehensions[H](fields, query)
    }

  implicit def typedAsComprehension[H <: HList](arg: ProjectedTypedTableComprehensions[H]):
  AsComprehension[ProjectedTypedTableComprehensions[H], ProjectedTypedTableComprehensions[H]] =
    new AsComprehension[ProjectedTypedTableComprehensions[H], ProjectedTypedTableComprehensions[H]](arg, arg.fields) {
      implicit val lenEnv = arg.lenEnv
      override def create(fields: Vector[Field], query: Atom): ProjectedTypedTableComprehensions[H] =
        new ProjectedTypedTableComprehensions[H](fields, query)
    }

  class TypedOptimizerComprehensions[H <: HList](private[extensions] val fields: Vector[Field], query: Atom)
                                           (implicit val lenEnv: hlist.Length[H])
    extends Rep[TypedTable[Tup[H]]] {

    def by(field: Symbol) =
      new ProjectedTypedTableComprehensions[H](
        fields,
        OrderBy(tree, Vector(FieldWithDirection(field, GroupBy.Asc)), isExperimentTarget = true)(new UntypedTableType))

    private def fromTypedTableComprehension[T <: HList](t: ProjectedTypedTableComprehensions[T]) = {
      implicit val lenEv = t.lenEnv
      new TypedOptimizerComprehensions[T](t.fields, t.query)
    }
    lazy val typedComp = new ProjectedTypedTableComprehensions[H](fields, query)

//    def flatMap(f: (Rep[Tup[H]]) => Rep[UntypedTable]): Rep[UntypedTable] = typedComp.flatMap(f)
    //TODO implement regular flatmap
    def flatMap[T <: HList](f: Rep[Tup[H]] => Rep[TypedTable[Tup[T]]])(implicit lenEnv: hlist.Length[T]) =
      fromTypedTableComprehension(typedComp.flatMap(f))

    def map[T <: HList](f: (Rep[Tup[H]]) => Rep[Tup[T]])(implicit lenEnv: hlist.Length[T]) =
      fromTypedTableComprehension(typedComp.map(f))

//    val withFilter: ((Rep[Tup[Nothing]]) => Rep[Boolean]) => ProjectedTypedTableComprehensions[H] = _
    //TODO implement withfilter

    override val tree: TTree = query
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

trait BaseRelationalCompilers extends Symbols with Queries with TableUtils {

  sealed trait RelationalCompilerPartBase {

    type NodeType
    protected var emittedQueries: Map[Sym, NodeType] = Map.empty
    protected val queryEnv: Map[Sym, NodeType]
    protected def fromCache(sym: Sym) = queryEnv.get(sym).orElse(emittedQueries.get(sym))

    final protected def compileTable(expr: Expression): NodeType = expr match {
      case Some(sym) /> _ if fromCache(sym).isDefined =>
        val op = fromCache(sym).get
        emittedQueries += ((sym, op))
        op
      case Some(sym) /> Query(q) =>
        val op = compileQuery(q)
        emittedQueries += ((sym, op))
        op
    }

    def compileQuery(q: Query): NodeType
  }

  abstract class CompileRelational extends RelationalCompilerPartBase {
    def apply(root: Expression): Writer[Map[Sym, NodeType], NodeType] = {
      val result = compileTable(root)
      Writer(emittedQueries, result)
    }
  }
}

trait SQLCompilers extends BaseRelationalCompilers {
  trait SQLCompilerPart extends RelationalCompilerPartBase {
    override type NodeType = String

    override def compileQuery(q: Query): NodeType = q match {
      case LoadTableFromFs(path) =>
        s"$path"
      case Join((_, left), (_, right), typ, conditions) =>
        s"select * from (${compileTable(left)}) join (${compileTable(right)})"
      case Transform(gen: Generator, source, _/>Pure(_/>(select: TupleConstructor))) =>
        val sourceOp = compileTable(source)

        val env = gen.symsToFields.andThen(_.name)
        val transforms = compilePureRow(select)(env).zip(select.names).map
        { case (expr, name) => s"$expr as $name" }

        s"select ${transforms.mkString(", ")} from ($sourceOp)"
    }

    private def compilePureRow(tup: TupleConstructor): ((Sym => String) => Vector[String]) = (env: Sym => String) =>
      tup.tuple.map {
        case _ /> link => ???
        case s: Sym => s"""`${env(s)}`"""
      }
  }

  class CompileSQL(override val queryEnv: Map[Sym, String] = Map.empty)
    extends CompileRelational with SQLCompilerPart

}
