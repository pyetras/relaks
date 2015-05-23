package relaks.lang.dsl.extensions.ast

import relaks.lang.ast._
import relaks.lang.dsl.AST._

/**
 * Created by Pietras on 23/05/15.
 */
sealed trait TableQuery extends Query {
  override def tpe: TType = new UntypedTableType
}

sealed case class Project(table: TTree, fields: Vector[Symbol]) extends TableQuery
sealed case class Transform(generator: TupleConstructor, table: TTree, select: TTree) extends TableQuery

sealed case class Join(left: TTree, right: TTree, typ: JoinType, conditions: Option[(TupleConstructor, TTree)]) extends TableQuery
sealed trait JoinType
object CartesianJoin extends JoinType
object InnerJoin extends JoinType

sealed case class Limit(table: TTree, start: TTree, count: TTree) extends TableQuery
sealed case class Filter(generator: TupleConstructor, table: TTree, filter: TTree) extends TableQuery
sealed case class GroupBy(generator: TupleConstructor, table: TTree, group: TTree) extends TableQuery
sealed case class Pure(value: TTree) extends TableQuery
