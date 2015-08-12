package relaks.lang.dsl.extensions.ast

import relaks.lang.ast._
import relaks.lang.dsl.AST._
import relaks.lang.dsl.Rep
import relaks.lang.dsl.utils.TreePrettyPrintable
import shapeless.{HList, HNil}

import scala.languageFeature.implicitConversions
import scalaz.Reader
import scalaz.Scalaz
import Scalaz._

/**
 * Created by Pietras on 12.08.15.
 */
trait Queries extends Symbols with ToTypedTreeOps {
  object Project {
    def apply(table: Atom, map: Vector[(Symbol, Symbol)]): Transform = {
      val generator = Generator.fromFields(map.map(_._1))
      val names = map.map(_._2.name)
      Transform(generator, table, RowRep(generator.toTupleWithNames[HNil](names)).tree)
    }
  }

  type SymTables = Map[Sym, Symbol]
  private val dups = (col: SymTables) => col.groupBy(_._2).mapValues(_.keys.toVector).filter(_._2.size > 1)

  class Generator(val symsToFields: SymTables, syms: Option[Vector[Sym]] = None) extends GeneratorBase {
    assert(dups(symsToFields).isEmpty, "generator contains duplicated fields")

    lazy val symsVector = syms.getOrElse(symsToFields.keys.toVector)

    def fields = symsToFields.values

    private def fuseWith(other: Generator): Generator = new Generator(symsToFields ++ other.symsToFields)

    lazy val duplicates: Map[Symbol, Vector[Sym]] = symsToFields.groupBy(_._2).mapValues(_.keys.toVector).filter(_._2.size > 1)

    def contains(sym: Sym) = symsToFields.contains(sym)

    def toTuple[F <: HList]: Rep[Tup[F]] = {
      new Rep[Tup[F]] {
        override val tree: Expression = TupleConstructor(symsVector) //TODO type
      }
    }

    def toTupleWithNames[F <: HList](names: Vector[String]): Rep[Tup[F]] = {
      new Rep[Tup[F]] {
        override val tree: TTree = TupleConstructor(symsVector, names)
      }
    }

    def update(updt: Map[Symbol, Symbol]) = new Generator(symsToFields.mapValues(field => updt.getOrElse(field, field)))

    override def toString: String = s"Generator[${symsToFields.values.mkString(", ")}]"
  }

  object Generator {
    def apply(syms: Vector[Sym], fields: Vector[Symbol]) = {
      new Generator((syms zip fields).toMap, syms.some)
    }
    def fromFields(fields: Vector[Symbol]) = apply(fields.indices.map(_ => fresh).toVector, fields)
    def unapply(generator: Generator): Option[(Iterable[Sym], Iterable[Symbol])] = generator.symsToFields.unzip.some

    /**
     * fuse two generators from different tables into the same table
     *
     * @param leftTable
     * @param leftGen
     * @param rightTable
     * @param rightGen
     * @return ((update, right table), generator)
     */
    def fuse(leftTable: Query, leftGen: Generator, rightTable: Query, rightGen: Generator): ((Map[Symbol, Symbol], Query), Generator) = {
      val sum = leftGen.symsToFields ++ rightGen.symsToFields
      val duplicates = dups(sum)

      def rename(field: Symbol, sym: Sym): Symbol = {
        //TODO something safer
        Symbol(s"${field.name}___${sym.name}")
      }
      val update = duplicates.mapValues(_.filter(sym => rightGen.contains(sym)))
        .flatMap { case (field, syms) => syms.map(sym => (sym, rename(field, sym))) }

      (if (update.nonEmpty) {
        //generate projection for right table
        val fields: Map[Symbol, Symbol] = rightGen.symsToFields
          .filter(symField => update.contains(symField._1))
          .map(symField => (symField._2, update(symField._1)))
        val projection = Project(rightTable, fields.toVector)
        (fields, projection)
      } else {
        (Map.empty, rightTable)
      }, new Generator(sum ++ update))
    }


    /**
     * merges two generators from the same table
     * if there are syms referring to fields with repeated names it chooses just one
     *
     * @param leftGen
     * @param rightGen
     * @return
     */
    def merge(leftGen: Generator, rightGen: Generator): Generator = {  //this should always be of size == 1
      new Generator((leftGen.symsToFields ++ rightGen.symsToFields).groupBy(_._2).mapValues(_.keys.head).groupBy(_._2).mapValues(_.keys.head))
    }
  }

  object GenPlusFilter {
    def unapply(opt: Option[(GeneratorBase, Atom)]) = for {
      (g: Generator, filter) <- opt
    } yield (g, filter)
  }

  object Query {
    def unapply(expr: Expression): Option[Query] = expr match {
      case _/>(q : Query) => q.some
      case _ => None
    }
  }

  object NextTable {
    def unapply(expr: Expression): Option[Atom] = expr match {
      case _/> Query(q) => q.stepTable
      case _ => None
    }
  }

  object Sources {
    def unapply(expr: Expression): Option[Seq[Atom]] = expr match {
      case _/> Query(q) => q.sources.some
      case _ => Seq.empty[Atom].some
    }
  }

  //if cannot get next table its a source
  object SourceTable {
    def unapply(expr: Expression): Option[Query] = NextTable.unapply(expr).isEmpty ? none[Query] | Query.unapply(expr)
  }

  private object InnerQuery {
    def unapply(expr: Expression): Option[Expression] = expr match {
      case _/>Transform(_, _, select) => select.some
      case _/>Filter(_, _, filter) => filter.some
      case _/>GroupBy(_, _, group) => group.some
      case _ => None
    }
  }

  object QueryWithGenerator {
    def unapply(expr: Atom): Option[(Generator, Atom)] = expr match {
      case t @ (_/> Transform(g: Generator, _, _)) => (g, t).some
      case f @ (_/> Filter(g: Generator, _, _)) => (g, f).some
      case group @ (_/> GroupBy(g: Generator, _, _)) => (g, group).some
      case _ => None
    }
  }

  class QueryPrettyPrintable(q: Query) extends TreePrettyPrintable {
    override def printVerbose: Rdr = {
      val printSubs: Rdr =
        q.sources.foldLeft(noop)((rdr, source) => rdr.flatMap(_ => source match {
          case _ /> Query(sourceq) => sourceq.printVerbose
        }))

      for {
        _ <- println(q.mainToString)
        _ <- indent {
          printSubs
        }
      } yield ()
    }
  }

  implicit def exprPrettyPrintable(expr: Expression): TreePrettyPrintable = expr match {
    case _/>Query(q) => new QueryPrettyPrintable(q)
    case _ => new TreePrettyPrintable {
      override def printVerbose: Rdr = Reader(x => x._1 ++= expr.toString)
    }
  }

  object RowRep {
    def apply[T <: HList](t: Rep[Tup[T]]) = new Rep[Table] {
      override val tree: Expression = Pure(t.tree)(new UntypedTableType)
    }
  }

}
