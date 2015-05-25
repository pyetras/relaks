package relaks.lang.dsl.extensions.ast

import relaks.lang.ast._
import relaks.lang.dsl.AST._
import relaks.lang.dsl.utils.PrettyPrintable

import scalaz.Reader
import scalaz.Scalaz._
import scalaz._

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
