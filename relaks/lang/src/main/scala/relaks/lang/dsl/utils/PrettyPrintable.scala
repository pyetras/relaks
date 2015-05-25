package relaks.lang.dsl.utils

import scalaz._

/**
 * Created by Pietras on 25/05/15.
 */
trait PrettyPrintable {
  protected type PrintingEnv = (StringBuilder, Int)
  protected type Rdr = Reader[PrintingEnv, Unit]

  protected def indent(block: => Rdr): Rdr = Reader(builderIndent => block((builderIndent._1, builderIndent._2 + 1)))
  private def print(str: String) : Rdr = Reader(builderIndent => builderIndent._1 ++= str)

  protected def println(str: String): Rdr = {
    for {
      _ <- printIndent
      _ <- print(str)
      _ <- print("\n")
    } yield ()
  }

  protected def noop: Rdr = Reader(_ => ())

  private def printIndent: Rdr = Reader(builderIndent => 0 until builderIndent._2 foreach (_ => print("  ")(builderIndent)))
}

trait TreePrettyPrintable extends PrettyPrintable {
  def printVerbose: Rdr
  def verboseString: String = {
    val sb = new StringBuilder
    printVerbose((sb, 0))
    sb.toString().dropRight(1)
  }

}