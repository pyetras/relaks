package fwb.parser.printers

import fwb.parser.ast.{Constants, AST}

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
  private def printList(lst: Traversable[Tree], sep: String) : Rdr = lst match {
    case h1 :: h2 :: tail => val pp = for {
      _ <- traverse(h1)
      _ <- print(sep)
      _ <- traverse(h2)
      } yield ()
      for {
        _ <- pp
        _ <- printList(tail, sep)
      } yield ()
    case h::Nil => for {
      _ <- traverse(h)
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
    case Literal(c) if c.tag === Constants.StringTag => for {
      _ <- print("\"")
      _ <- print(c.v.toString)
      _ <- print("\"")
    } yield ()
    case Literal(c) => for {
      _ <- print(c.v.toString)
    } yield ()
    case Apply(Operator(op), List(left, right)) => for {
      _ <- traverse(left)
      _ <- print(s" $op ")
      _ <- traverse(right)
    } yield ()
    case expr: Latin => expr match {
      case Foreach(rels, stmts) => for {
        _ <- print("FOREACH ")
        _ <- printList(rels.list, ", ") // TODO: optimize .list
        _ <- print("\n")
        _ <- indent({ printList(stmts.list, "") }) // TODO: optimize .list
      } yield ()
    }

  }
}
