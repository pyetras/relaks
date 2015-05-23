package relaks.lang.dsl.extensions.ast

import relaks.lang.ast._
import relaks.lang.dsl.AST._

/**
 * Created by Pietras on 23/05/15.
 */
sealed trait TableQuery extends Query {
  override def tpe: TType = new UntypedTableType
}
trait GeneratorBase

sealed case class Transform(generator: GeneratorBase, table: Atom, select: Atom) extends TableQuery

sealed case class Join(left: Atom, right: Atom, typ: JoinType, conditions: Option[(GeneratorBase, Atom)]) extends TableQuery
sealed trait JoinType
object CartesianJoin extends JoinType
object InnerJoin extends JoinType

sealed case class Limit(table: Atom, start: Atom, count: Atom) extends TableQuery
sealed case class Filter(generator: GeneratorBase, table: Atom, filter: Atom) extends TableQuery
sealed case class GroupBy(generator: GeneratorBase, table: Atom, group: Atom) extends TableQuery
sealed case class Pure(value: Atom) extends TableQuery
