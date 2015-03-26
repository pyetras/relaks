package fwb.parser.printers

import fwb.parser.ast.{Constants, AST}

import scalaz.{Reader, Scalaz}

/**
 * Created by Pietras on 24/03/15.
 */
class PrettyPrinter{
  import Scalaz._
  import AST._

  def apply(tree: Tree) : String = {
    val buf = new StringBuilder
    traverse(tree)(buf)
    buf.toString()
  }

  def print(str: String) : Reader[StringBuilder, Unit] = Reader((builder :StringBuilder) =>
    builder ++= str)

  def traverse(tree: Tree) : Reader[StringBuilder, Unit] = tree match {
    case Program(statements) => Reader((state) => {
      statements.foreach(stmt => traverse(stmt)(state))
    })
    case stmt: Statement =>
      val printStmt = stmt match {
        case Assignment(left, right) => for {
          _ <- traverse(left)
          _ <- print(" = ")
          _ <- traverse(right)
        } yield ()
        case NoOp => Reader((s:StringBuilder) => ())
      }
      for {
        _ <- printStmt
        _ <- print(";\n")
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
  }
}
