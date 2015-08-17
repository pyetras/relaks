package relaks.lang.phases.interpreter

import java.sql.Statement

import relaks.lang.ast.Expression
import relaks.lang.dsl.extensions.ast.logical.QueryOp.QueryOp
import relaks.lang.dsl.extensions.ast.logical.QueryOp.QueryOp
import relaks.lang.dsl.extensions.ast.logical.{QueryOp, LoadComprehension, SelectComprehension}
import relaks.lang.dsl.extensions.{TupleInterpreter, SuperPosAnalysis}
import relaks.lang.impl.Row
import relaks.lang.phases.rewriting.QueryRewritingPhases
import relaks.optimizer.NondetParams
import relaks.lang.impl
import relaks.lang.ast._
import scalikejdbc._

import scala.annotation.tailrec
import scalaz._
import scalaz.concurrent.Task
import scalaz.stream._
import util.control.Exception._
import Scalaz._

/**
 * Created by Pietras on 16.08.15.
 */
trait DrillInterpreter extends ComprehensionInterpreter {
  import QueryOp._

  lazy val connection = {
    Class.forName("local.org.apache.drill.jdbc.Driver")
    ConnectionPool.singleton("jdbc:drill:drillbit=localhost", "", "")
    ConnectionPool.borrow()
  }

  //collection of symbols from ops until first transform (inclusive)
  private[lang] def initialOps(seq: Seq[QueryOp]) = {
    @tailrec
    def go(s: Seq[QueryOp], acc: Seq[QueryOp] = Seq.empty): Seq[QueryOp] = s match {
      case (t:Transform) +: _ => t +: acc
      case q +: rest => go(rest, q +: acc)
      case Seq() => acc
    }

    val initial = go(seq)

    val extract = (g: Generator) => g.symsToFields.iterator.map { case (sym, name) => name -> sym.tpe }

    val generators = initial.collect {
      case Transform(g: Generator, _) => extract(g)
      case Filter(g: Generator, _) => extract(g)
      case OrderBy(fields, _) => fields.map(f => f.field -> ScalaTypes.anyType)
    }
    generators.foldLeft(Set.empty[(Symbol, TType)]){_ ++ _}
  }

  private[lang] def makeCast(typ: TType): (Symbol => String) = {
        import ScalaTypes._
    (colname: Symbol) =>
      val drillt = typ match {
        case t if t == intType => "INT".some
        case t if t == stringType => None
        case t if t == doubleType => "DOUBLE".some
        case t if t == boolType => "BOOLEAN".some
//        case t if t == floatType => "FLOAT".some
        case _: NativeArgType[_] => "BINARY".some
        case _ => None
      }
      drillt.map(t => s"CAST(${colname.name} AS ${t})").getOrElse(colname.name)
  }


  private[lang] def toProjection(fields: Traversable[(Symbol, TType)]) = {
    if (fields.isEmpty) "*"
    else {
      fields.map({
        case (name, typ) => makeCast(typ)(name)
      }).mkString(", ")
    }
  }

  private[lang] val rowsProcess: PartialFunction[Expression, Process[Task, impl.Row]] = {
    case _/> SelectComprehension(LoadComprehension(LoadTableFromFs(path)), transforms, filters, limits, orderBys, seq) =>
      implicit val session = new ActiveSession(connection, DBConnectionAttributes())

      val projectSet = initialOps(seq)
      assert(projectSet.map(_._1).size == projectSet.size, "multiple fields of different types currently not supported")

      val projection = toProjection(projectSet)

      val query = s"SELECT ${projection} FROM dfs.`${path}`"

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

      if (projection != "*"){
        rows |> process1.lift { wrs =>
          new impl.Row((1 to projectSet.size).map(i => wrs.any(i)).toVector, projectSet.toVector.map(x => x._1.name -> x._2))
        }
      } else {
        rows |> process1.lift { wrs =>
          val row = wrs.array(0)
          val rs = row.getResultSet
          val ncols = rs.getMetaData.getColumnCount
          val (vals, schema) = (1 to ncols).map(i => wrs.any(i) -> (s"columns[$i]" -> ScalaTypes.anyType)).toVector.unzip
          new impl.Row(vals, schema)
        }
      }
  }

  val operatorsProcess: PartialFunction[Expression, Process1[impl.Row, impl.Row]] = {
    case _/> SelectComprehension(_, _, _, _, _, seq) =>
      seq.foldl(process1.id[impl.Row])(acc => op => acc |> evalQuery(op))
  }

  val id = implicitly[Arrow[PartialFunction]].id[Expression]

  val generatorPlusOps = (id &&& id) >>> rowsProcess *** operatorsProcess >>> { case (generator, ops) => generator |> ops }

  override protected[lang] def evalComprehension(expr: Expression): Process[Task, Row] =
    generatorPlusOps.applyOrElse(expr, super.evalComprehension)
}
