package relaks.lang.parser.printers

import relaks.lang.parser.AST

import scala.annotation.tailrec
import scalaz.{Reader, Scalaz}

/**
 * Created by Pietras on 24/03/15.
 */
class PrettyPrinter{
  import Scalaz._
  import AST._

  def apply(tree: Tree) : String = {
    val buf = new StringBuilder
    traverse(tree)((buf, 0))
    buf.toString()
  }

  type Env = (StringBuilder, Int)
  type Rdr = Reader[Env, Unit]

  private def print(str: String) : Rdr = Reader((pair: Env) => pair match
    { case (builder, _) => builder ++= str })

  private def nothing = print("")

  // TODO: not tailrec
  private def printList[T](lst: Traversable[T], sep: String, printer: T => Rdr = traverse _) : Rdr = lst match {
    case h1 :: h2 :: tail => val pp = for {
      _ <- printer(h1)
      _ <- print(sep)
      } yield ()
      for {
        _ <- pp
        _ <- printList(h2::tail, sep, printer)
      } yield ()
    case h::Nil => for {
      _ <- printer(h)
    } yield ()
    case Nil => nothing
  }

  private def indent(block: => Rdr): Rdr =
    Reader((pair: Env) => pair match { case (sb, i) => block((sb, i+1)) })

  private def printIndent: Rdr = Reader((pair: Env) => pair match {
    case (sb, i) => {
      0 until i foreach (_ => print("\t")((pair)))
      ()
    }
  })

  def traverse(tree: Tree) : Rdr = tree match {
    case Program(statements) => Reader((state) => {
      statements.foreach(stmt => traverse(stmt)(state))
    })
    case stmt: Statement =>
      val printStmt = stmt match {
        case Assignment(left, right) => for {
          _ <- traverse(left)
          _ <- print(" = ")
          _ <- traverse(right)
        } yield false
        case NoOp => Reader((s: Env) => false)
        case Generate(rels) => for {
          _ <- print("GENERATE ")
          _ <- printList(rels.list, ", ") // TODO: optimize .list
        } yield true
      }
      for {
        _ <- printIndent
        isGenerate <- printStmt
        _ <- if (!isGenerate) print(";\n") else nothing
      } yield ()
    case Identifier(name) => for {
      _ <- print(name)
    } yield ()
    case Literal(v) =>
      v match {
        case _: String => for {
          _ <- print("\"")
          _ <- print(v.toString)
          _ <- print("\"")
        } yield ()
        case _: List[_] => for {
          _ <- print("[")
          _ <- printList(v.asInstanceOf[List[Expression]], ", ")
          _ <- print("]")
        } yield ()
        case _ => for {
          _ <- print(v.toString)
        } yield ()
      }
    case expr: Expression => expr match {
      case Apply(Operator(op), List(left, right)) => for {
        _ <- print("(")
        _ <- traverse(left)
        _ <- print(s" $op ")
        _ <- traverse(right)
        _ <- print(")")
      } yield ()
      case Apply(Identifier(name), lst) => for {
        _ <- print(name)
        _ <- print("(")
        _ <- printList(lst, ", ")
        _ <- print(")")
      } yield ()
      case Select(lhs, rhs) => for {
        _ <- print("(")
        _ <- traverse(lhs)
        _ <- print(".")
        _ <- traverse(rhs)
        _ <- print(")")
      } yield ()
      case latin: Latin => latin match {
        case Foreach(rels, stmts) => for {
          _ <- print("FOREACH ")
          _ <- printList(rels.sequenceU.right.get.list, ", ") // TODO: optimize .list
          _ <- print("\n")
          _ <- indent({
            printList(stmts.list, "")
          }) // TODO: optimize .list
        } yield ()
        case Limit(Right(rel), lim) => for {
          _ <- print("LIMIT ")
          _ <- traverse(rel)
          _ <- print(" ")
          _ <- traverse(lim)
        } yield ()
        case Where(Right(rel), cond) => for {
          _ <- print("FILTER ")
          _ <- traverse(rel)
          _ <- print(" BY ")
          _ <- traverse(cond)
        } yield ()
        case ColumnOrder(Right(rel), dirs) => for {
          _ <- print("ORDER ")
          _ <- traverse(rel)
          _ <- print(" BY ")
          _ <- printList(dirs.list, ", ", (x: (Expression, OrderDirection)) => for {// TODO: optimize .list
            _ <- traverse(x._1)
            _ <- print(" ")
            _ <- print(if (x._2 == Asc) "ASC" else "DESC")
          } yield ())
        } yield ()
        case Search(rels, typ, stmts) => for {
          _ <- print(if (typ == Grid) "GRID " else "")
          _ <- print("SEARCH ")
          _ <- printList(rels.list, ", ") // TODO: optimize .list
          _ <- print("\n")
          _ <- indent({
            printList(stmts.list, "")
          }) // TODO: optimize .list
        } yield ()
      }
    }
  }
}
