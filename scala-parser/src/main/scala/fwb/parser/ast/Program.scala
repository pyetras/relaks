package fwb.parser.ast

/**
 * Created by Pietras on 22/03/15.
 */
object Program {
  implicit class Program(val list: Traversable[ASTNode]){
    override def toString = list.map(_.toString()).mkString("\n")
  }
}
