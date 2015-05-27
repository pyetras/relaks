package relaks.lang.dsl.extensions.ast

import relaks.lang.ast._
import relaks.lang.dsl.AST._
import relaks.lang.dsl.{Rep, Symbols}
import relaks.lang.dsl.utils.{TreePrettyPrintable, PrettyPrintable}
import shapeless.{HNil, HList}
import relaks.lang.dsl.AST.syntax._

import scalaz.Reader
import scalaz.Scalaz._
import scalaz._
import scala.language.implicitConversions

/**
 * Created by Pietras on 23/05/15.
 */

sealed trait Query extends Expression with PrettyPrintable {
  def stepTable: Option[Atom]
  def sources: Seq[Atom]
}


sealed trait TableQuery extends Query
trait GeneratorBase {
//  def unifyWith(other: GeneratorBase): GeneratorBase
}

sealed trait SingleSourceTransformation extends Query {
  override def sources: Seq[Atom] = stepTable.toSeq
}

sealed case class LoadTableFromFs(path: String) extends TableQuery {
  override def mainToString: String = withArgs(super.mainToString, path)

  override def stepTable: Option[Atom] = None
  override def sources: Seq[Atom] = Seq.empty
}

sealed case class Transform(generator: GeneratorBase, table: Atom, select: Atom) extends Query with SingleSourceTransformation {
  override def mainToString: String = withArgs(super.mainToString, select.toString)

  override def stepTable: Option[Atom] = table.some
}

sealed case class Join(left: (GeneratorBase, Atom), right: (GeneratorBase, Atom), typ: JoinType, conditions: Option[(GeneratorBase, Atom)]) extends TableQuery {
  override def mainToString: String = withArgs(super.mainToString, (typ.toString +: conditions.toSeq.map(_._2.toString)):_*)

  override def stepTable: Option[Atom] = None
  override def sources: Seq[Atom] = Seq(left._2, right._2)
}
sealed trait JoinType {
  override def toString: String = this.getClass.getSimpleName
}
object CartesianJoin extends JoinType
object InnerJoin extends JoinType

sealed case class Limit(table: Atom, start: Atom, count: Atom) extends TableQuery {
  override def mainToString: String = withArgs(super.mainToString, count.toString)

  override def stepTable: Option[Atom] = None
  override def sources: Seq[Atom] = Seq(table)
}
sealed case class Filter(generator: GeneratorBase, table: Atom, filter: Atom) extends Query with SingleSourceTransformation {
  override def mainToString: String = withArgs(super.mainToString, filter.toString)

  override def stepTable: Option[Atom] = table.some
}
sealed case class GroupBy(generator: GeneratorBase, table: Atom, group: Atom) extends Query with SingleSourceTransformation {
  override def mainToString: String = withArgs(super.mainToString, group.toString)

  override def stepTable: Option[Atom] = table.some
}

sealed case class Pure(value: Atom) extends Expression

trait Queries extends Symbols {
  object Project {
    def apply(table: Atom, map: Vector[(Symbol, Symbol)]): Transform = {
      val generator = Generator.fromFields(map.map(_._1))
      Transform(generator, table, RowRep(generator.toTuple[HNil]).tree)
    }
  }

  type SymTables = Map[Sym, Symbol]

  class Generator(val symsToFields: SymTables, syms: Option[Vector[Sym]] = None) extends GeneratorBase {
    lazy val symsVector = syms.getOrElse(symsToFields.keys.toVector)

    def unifyWith(other: Generator): Generator = new Generator(symsToFields ++ other.symsToFields)

    def duplicates: Map[Symbol, Vector[Sym]] = symsToFields.groupBy(_._2).mapValues(_.keys.toVector).filter(_._2.size > 1)

    def contains(sym: Sym) = symsToFields.contains(sym)

    def toTuple[F <: HList]: Rep[Tup[F]] = {
      new Rep[Tup[F]] {
        override val tree: Expression = TupleConstructor(symsVector) //TODO type
      }
    }

    override def toString: String = s"Generator[${symsToFields.values.mkString(", ")}]"
  }

  implicit val generatorSemigroup: Semigroup[Generator] = new Semigroup[Generator] {
    override def append(f1: Generator, f2: => Generator): Generator = f1.unifyWith(f2)
  }

  object Generator {
    def apply(syms: Vector[Sym], fields: Vector[Symbol]) = {
      new Generator((syms zip fields).toMap, syms.some)
    }
    def fromFields(fields: Vector[Symbol]) = apply(fields.indices.map(_ => fresh).toVector, fields)
    def unapply(generator: Generator): Option[(Iterable[Sym], Iterable[Symbol])] = generator.symsToFields.unzip.some
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

  object StepTable {
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