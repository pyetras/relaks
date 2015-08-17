package relaks.lang.phases.interpreter

import java.sql.Statement

import relaks.lang.ast.Expression
import relaks.lang.dsl.extensions.ast.logical.{LoadComprehension, SelectComprehension}
import relaks.lang.dsl.extensions.{TupleInterpreter, SuperPosAnalysis}
import relaks.lang.phases.rewriting.QueryRewritingPhases
import relaks.optimizer.NondetParams
import relaks.lang.impl
import relaks.lang.ast._
import scalikejdbc._

import scalaz._
import scalaz.concurrent.Task
import scalaz.stream._
import util.control.Exception._
import Scalaz._

/**
 * Created by Pietras on 16.08.15.
 */
trait DrillInterpreter extends Environments
with NondetParams
with SuperPosAnalysis
with BaseExprInterpreter
with SuperPosInterpreter
with BaseQueryOpInterpreter
with QueryRewritingPhases
with TupleInterpreter {

  private[lang] val rowsProcess: (Expression) => Process[Task, WrappedResultSet] = {
    case _/> SelectComprehension(LoadComprehension(LoadTableFromFs(path)), transforms, filters, limits, orderBys, seq) =>
//      Class.forName("org.apache.calcite.jdbc.Driver")
      Class.forName("local.org.apache.drill.jdbc.Driver")
      ConnectionPool.singleton("jdbc:drill:drillbit=localhost", "", "")
      val connection = ConnectionPool.borrow()

//      val calciteConnection: CalciteConnection = connection.unwrap(classOf[CalciteConnection])
//      val drill = JdbcSchema.dataSource("jdbc:drill:drillbit=localhost", "local.org.apache.drill.jdbc.Driver", "", "")
//      calciteConnection.getRootSchema.add("drill", JdbcSchema.create(calciteConnection.getRootSchema, "drill", drill, null, null))

      implicit val session = new ActiveSession(connection, DBConnectionAttributes())

      val query = s"SELECT * FROM dfs.`${path}` LIMIT 20"

      val executorS: Process[Task, Statement] = io
        .resource(Task.delay(connection.createStatement()))(
          stmt =>
            Task.delay {
              ignoring(classOf[Throwable]) {
                stmt.close()
              }
            }
        )(stmt =>
          Task.delay(stmt)
        )

      val rows: Process[Task, WrappedResultSet] = executorS.take(1).flatMap { stmt => io.resource(
        Task.delay(stmt.executeQuery(query))
      )(rs =>
        Task.delay {
          ignoring(classOf[Throwable]) {
            rs.close()
          }
        }
        )(rs =>
        Task.delay {
          if (rs.next()) {
            new WrappedResultSet(rs, new ResultSetCursor(0), 0)
          } else throw Cause.Terminated(Cause.End)
        }
        )
      }

      rows
  }

}
