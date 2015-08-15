package relaks.lang.dsl.extensions

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.core.JsonParser.Feature
import com.fasterxml.jackson.databind.{SerializationFeature, ObjectMapper}
import com.fasterxml.jackson.databind.module.SimpleModule
import com.typesafe.scalalogging.LazyLogging
import org.apache.calcite.rel.core.JoinRelType
import org.apache.drill.common.JSONOptions
import org.kiama.attribution.Attribution
import org.kiama.relation.GraphTree
import org.apache.drill.common.expression.{SchemaPath, LogicalExpression, FieldReference}
import org.apache.drill.common.logical.data.{Transform => DrillTransform, Join => DrillJoin, NamedExpression, JoinCondition, LogicalOperator, Scan}
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
    extends Rep[UntypedTable] with TableComprehensions {

    override type ComprehensionsMonad = ProjectedTableComprehensions[FieldsLen]
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

    override protected[extensions] def filterImpl[F <: HList](f: (Rep[Tup[F]]) => Rep[Boolean], fields: Vector[Field]) = {
      val (generator, rep) = tupleGenerator(fields)
      val cond = f(rep).tree
      createFilterComprehension(generator, cond)
    }

    def filter[F <: HList](f: Rep[Tup[F]] => Rep[Boolean])(implicit lenEv: hlist.Length.Aux[F, FieldsLen]) = {
      filterImpl(f, fields)
    }

    override protected[extensions] def orderImpl(_fieldsVec: Vector[Field], expr: OrderBy): ProjectedTableComprehensions[FieldsLen] = {
      new ProjectedTableComprehensions[FieldsLen](fields, expr)
    }
  }

  class ProjectedTypedTableComprehensions[H <: HList](private[extensions] val fields: Vector[Field], private[extensions] val query: Atom)
                                                                (implicit val lenEnv: hlist.Length[H])
    extends Rep[TypedTable[Tup[H]]] with TableComprehensions {

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
    def filter(f: Rep[Tup[H]] => Rep[Boolean]) = fromUntypedTableComprehension(untypedComprehension.filter(f))

    override type ComprehensionsMonad = ProjectedTypedTableComprehensions[H]

    override protected[extensions] def filterImpl[F <: HList](f: (Rep[Tup[F]]) => Rep[Boolean], fields: Vector[Field]) =
      fromUntypedTableComprehension(untypedComprehension.filterImpl(f, fields))

    override protected[extensions] def orderImpl(fieldsVec: Vector[Field], expr: OrderBy) =
      fromUntypedTableComprehension(untypedComprehension.orderImpl(fieldsVec, expr))

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

  implicit def untypedAsComprehension(arg: Rep[UntypedTable]): AsComprehension[Rep[UntypedTable], Rep[UntypedTable]] = new AsComprehension[Rep[UntypedTable], Rep[UntypedTable]](arg, null) {
    override def create(fields: Vector[Field], query: Atom): Rep[UntypedTable] = new Rep[UntypedTable] {
      override val tree: TTree = query
    }
  }

  implicit def typedOptimizerAsComprehension[H <: HList](arg: TypedOptimizerComprehensions[H]): AsComprehension[TypedOptimizerComprehensions[H], TypedOptimizerComprehensions[H]] =
    new AsComprehension[TypedOptimizerComprehensions[H], TypedOptimizerComprehensions[H]](arg, arg.fields) {
      implicit val lenEnv = arg.lenEnv
      override def create(fields: Vector[Field], query: Atom): TypedOptimizerComprehensions[H] = new TypedOptimizerComprehensions[H](fields, query)
    }

  implicit def typedAsComprehension[H <: HList](arg: ProjectedTypedTableComprehensions[H]): AsComprehension[ProjectedTypedTableComprehensions[H], ProjectedTypedTableComprehensions[H]] =
    new AsComprehension[ProjectedTypedTableComprehensions[H], ProjectedTypedTableComprehensions[H]](arg, arg.fields) {
      implicit val lenEnv = arg.lenEnv
      override def create(fields: Vector[Field], query: Atom): ProjectedTypedTableComprehensions[H] = new ProjectedTypedTableComprehensions[H](fields, query)
    }

  class TypedOptimizerComprehensions[H <: HList](private[extensions] val fields: Vector[Field], query: Atom)
                                           (implicit val lenEnv: hlist.Length[H])
    extends Rep[TypedTable[Tup[H]]] with TableComprehensions {

    override type ComprehensionsMonad = TypedOptimizerComprehensions[H]

    def by(field: Symbol) =
      typedComp.orderImpl(null, OrderBy(tree, Vector(FieldWithDirection(field, GroupBy.Asc)), isExperimentTarget = true)(new UntypedTableType))

    private def fromTypedTableComprehension[T <: HList](t: ProjectedTypedTableComprehensions[T]) =
      new TypedOptimizerComprehensions[H](t.fields, t.query)
    lazy val typedComp = new ProjectedTypedTableComprehensions[H](fields, query)

//    def flatMap(f: (Rep[Tup[H]]) => Rep[UntypedTable]): Rep[UntypedTable] = typedComp.flatMap(f)
    //TODO implement regular flatmap
    def flatMap[T <: HList](f: Rep[Tup[H]] => Rep[TypedTable[Tup[T]]])(implicit lenEnv: hlist.Length[T]) =
      fromTypedTableComprehension(typedComp.flatMap(f))

    def map[T <: HList](f: (Rep[Tup[H]]) => Rep[Tup[T]])(implicit lenEnv: hlist.Length[T]) =
      fromTypedTableComprehension(typedComp.map(f))

//    val withFilter: ((Rep[Tup[Nothing]]) => Rep[Boolean]) => ProjectedTypedTableComprehensions[H] = _
    //TODO implement withfilter
    def filter(f: (Rep[Tup[H]]) => Rep[Boolean]) = fromTypedTableComprehension(typedComp.filter(f))

    override protected[extensions] def filterImpl[F <: HList](f: (Rep[Tup[F]]) => Rep[Boolean], fields: Vector[Field]) =
      fromTypedTableComprehension(typedComp.filterImpl(f, fields))

    override protected[extensions] def orderImpl(fieldsVec: Vector[Field], expr: OrderBy) =
      fromTypedTableComprehension(typedComp.orderImpl(fieldsVec, expr))

    override val tree: TTree = query
  }

//  class UntypedOptimizerComprehensions(override val tree: Expression) extends Rep[UntypedTable] { self =>
//    def by(field: Symbol): Rep[UntypedTable] = new Rep[UntypedTable] {
//      override val tree: Atom = OrderBy(self.tree, Vector(FieldWithDirection(field, GroupBy.Asc)))(new UntypedTableType)
//    }
//  }

  trait OrderableTableComprehensions {
    type ComprehensionsMonad
    val tree: Expression
    protected[extensions] def orderImpl(fieldsVec: Vector[Field], expr: OrderBy): ComprehensionsMonad

    //TODO ordering types
    def orderBy[P <: Product](fields: P)(implicit toVector: ToTraversable.Aux[P, Vector, Symbol]): ComprehensionsMonad = {
      val fieldsVec = toVector(fields)
      val expr = OrderBy(tree, fieldsVec.map(FieldWithDirection(_, GroupBy.Asc)), isExperimentTarget = true)(new UntypedTableType)
      orderImpl(fieldsVec.map(Field(_, UnknownType)), expr)
    }

    def orderBy(field: Symbol): ComprehensionsMonad = {
      orderBy(Tuple1(field))
    }
  }

  trait TableComprehensions extends OrderableTableComprehensions {
    protected[extensions] def filterImpl[F <: HList](f: (Rep[Tup[F]]) => Rep[Boolean], fields: Vector[Field]): ComprehensionsMonad
    def filter[P <: Product, FL <: Nat, F <: HList](fields: P)(f: Rep[Tup[F]] => Rep[Boolean])(implicit
                                                                                               lenEv: tuple.Length.Aux[P, FL],
                                                                                               lenEnv2: hlist.Length.Aux[F, FL],
                                                                                               toVector: ToTraversable.Aux[P, Vector, Field]) =
      filterImpl(f, toVector(fields))

  }

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

  class TableOperations(arg1: Rep[UntypedTable]) extends OrderableTableComprehensions {

//    def apply[P <: Product](fields: P)(implicit factory: ProjectionMagnet[P]) = {
//      factory(fields, arg1)
//    }

    def apply[P <: Product, FieldsLen <: Nat, L <: HList, S <: HList](fields: P)(implicit tupEv: IsTuple[P],
                                                                      toHlist: Generic.Aux[P, L],
                                                                      schema: TypedSymbolSchema.Aux[L, S],
                                                                      lenEv: hlist.Length.Aux[S, FieldsLen],
                                                                      fieldsLength: ToInt[FieldsLen]) = {
      new ProjectedTypedTableComprehensions[S](Tag.unwrap(schema(toHlist.to(fields))), arg1.tree)
    }

    def apply[E](field: TypedField[E]): ProjectedTypedTableComprehensions[E :: HNil] =
      apply(Tuple1(field))

    override type ComprehensionsMonad = Rep[UntypedTable]
    override protected[extensions] def orderImpl(fieldsVec: Vector[Field], expr: OrderBy): ComprehensionsMonad = new Rep[UntypedTable] {
      override val tree: Atom = expr
    }

    override val tree: Expression = arg1.tree
  }

  implicit def addTableOps(t: Rep[UntypedTable]): TableOperations = new TableOperations(t)
  implicit def addTypedTableOps(t: Rep[TypedTable[_]]): TableOperations = new TableOperations(new Rep[UntypedTable] { override val tree = t.tree })

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

trait DrillCompilers extends BaseRelationalCompilers with Symbols with Queries with LazyLogging with TableUtils {
  private lazy val mapper = {
    val m = new ObjectMapper
    val deserModule: SimpleModule = new SimpleModule("LogicalExpressionDeserializationModule")
      .addDeserializer(classOf[LogicalExpression], new LogicalExpression.De(null))
      .addDeserializer(classOf[SchemaPath], new SchemaPath.De(null))
    m.registerModule(deserModule)
    m.enable(SerializationFeature.INDENT_OUTPUT)
    m.configure(Feature.ALLOW_UNQUOTED_FIELD_NAMES, true)
    m.configure(JsonGenerator.Feature.QUOTE_FIELD_NAMES, true)
    m.configure(Feature.ALLOW_COMMENTS, true)
    m
  }

  trait DrillCompilerPart extends RelationalCompilerPartBase {
    override type NodeType = LogicalOperator

    override def compileQuery(q: Query): LogicalOperator = q match {
      case LoadTableFromFs(path) =>
        val optionsJson = "{\"format\" : {\"type\" : \"parquet\"},\"files\" : [ \"file:"  + path + "\" ]}"
        val opts = mapper.readValue(optionsJson, classOf[JSONOptions])
        val scan = new Scan("dfs", opts)
        scan

      case Join((_, left), (_, right), typ, conditions) =>
        val leftOp = compileTable(left)
        val rightOp = compileTable(right)

        val joinT = typ match {
          case CartesianJoin => JoinRelType.INNER
          case InnerJoin => JoinRelType.INNER
        }

        new DrillJoin(leftOp, rightOp, Array.empty[JoinCondition], joinT)

      case Transform(gen: Generator, source, _/>Pure(_/>(select: TupleConstructor))) =>
        val sourceOp = compileTable(source)

        val env = gen.symsToFields.andThen(_.name)
        val transforms = compilePureRow(select)(env).zip(select.names).map
        { case (expr, name) => new NamedExpression(mapper.readValue(expr, classOf[LogicalExpression]), new FieldReference(name)) }

        val transform = new DrillTransform(transforms.toArray)
        transform.setInput(sourceOp)
        transform
    }

    private def compilePureRow(tup: TupleConstructor): ((Sym => String) => Vector[String]) = (env: Sym => String) =>
      tup.tuple.map {
        case _ /> link => ???
        case s: Sym => s""""`${env(s)}`""""
      }
  }

  class CompileDrill(override val queryEnv: Map[Sym, LogicalOperator] = Map.empty)
    extends CompileRelational with DrillCompilerPart

}



