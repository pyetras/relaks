package relaks.lang.phases.rewriting

import com.typesafe.scalalogging.LazyLogging
import org.kiama._
import org.kiama.attribution.Attribution
import org.kiama.relation.GraphTree
import relaks.lang.ast._
import relaks.lang.dsl.extensions.TableUtils
import relaks.lang.dsl.extensions.ast._
import relaks.lang.dsl.extensions.ast.logical.{Comprehension, LoadComprehension, QueryOp, SelectComprehension}
import scalaz.Scalaz
import Scalaz._

/**
 * Created by Pietras on 12.08.15.
 */
trait QueryRewritingPhases extends LazyLogging with Symbols with Queries with TableUtils {
  //note that it only works on syms
  //it also does not extract nested comprehensions (should be applied multiple times)
  object ComprehensionBuilder extends Attribution {
    val comprehension: Expression => Option[SelectComprehension] = attr {
      case Some(querySym) /> NextTable(nextSym) =>
        val _ /> query = querySym
        for {
          select <- comprehension(nextSym)
          updatedSelect <- query match {
            //            case l: Limit => QueryOp.unapply(l).map(op => select.appendAndCommit(op))
            case QueryOp(queryOp) => select.append(queryOp).some
            case gb: GroupBy => throw new NotImplementedError("GroupByComprehension not implemented")
            case join: Join => throw new NotImplementedError("JoinComprehension not implemented")
          }
        } yield updatedSelect
      case _ /> (source: SourceQuery) => SelectComprehension(LoadComprehension(source)).some
      case _ => None
    }
  }

  import org.kiama.rewriting.Rewriter._

  class LeafSyms(tree: GraphTree) extends Attribution { self =>
    val leafSyms: Expression => Set[Sym] = attr {
      case Expr(node) => tree.child(node).map(self.leafSyms).foldLeft(Set.empty[Sym]) {_ ++ _}
      case s: Sym => Set(s)
    }
  }

  object ClosestFilter extends Attribution {
    val closestFilter: Expression => Option[(Generator, Atom)] = attr {
      case _ /> Filter(gen: Generator, _, filter) => (gen, filter).some
      case _ /> Join(_, _, InnerJoin, GenPlusFilter(gen, filter)) => (gen, filter).some
      //      case _/>Project(_/> table, _) => table -> closestFilter
      case _ => None
    }
  }

  object OutputSchema extends Attribution {
    import QueryOp._
    val forTransform: Transform => Vector[(String, TType)] = attr {
      case Transform(_, _/>Pure(_/>(t: TupleConstructor))) => t.names.zip(t.tuple.map(_.tpe))
      case Transform(_, _/> (c: Comprehension)) => forComprehension(c)
    }

    val forComprehension: Comprehension => Vector[(String, TType)] = attr {
      case SelectComprehension(input, transforms, _, _, _, _) => this.forTransform(transforms.last)
    }
  }

//  def doAnalyze_(tree: Expression): ValidationNel[String, Unit] = tree match {
//    case Transform(Expr(TupleConstructor(in)), table, Expr(Pure(TupleConstructor(out)))) => //simple map comprehension - nothing to rewrite
//      //for each field, find out where it went and what transformations were applied
//      val ins = in.asInstanceOf[Vector[Sym]].toSet
//      val outs = out.map(expr => expr -> leafSyms)
//
//      val dropped = ins diff (outs.reduce(_ ++ _)) //TODO move this to analyze/rewrite phase
//      if (dropped.nonEmpty) {
//        logger.debug(s"${dropped.size} of projected fields not used")
//      }
//      ().successNel[String]
//    case _ => ().successNel[String]
//  }

  /**
   * Merges nested expressions into joins
   * TODO change argument to atom
   * @return
   */
  val fuseTransforms = repeat(oncetd(query[Expression](fuseTransformsImpl))) andThen (_.map(_.asInstanceOf[Expression]))
  private def fuseTransformsImpl : Expression ==> Unit = {
    //nested transformation whose result is a pure expression
    case Some(sym) /> Transform(gPar: Generator, Some(parSym) /> Query(parTable),
    _ /> Transform(gChild: Generator, Some(childSym) /> Query(childTable), _ /> (select@Pure(_)))) =>
      logger.debug("found candidates for a merge with pure output")

      //see if there is a filter that contains both parent and child syms
      //TODO split filters
      //TODO remove old filter from tree
      //returns merged generator for the right table and some filter
      val (gMerged, filter): (Generator, Option[(Generator, Atom)]) = ClosestFilter.closestFilter(childTable) map (filter => {
        val (gFilter, sel) = filter
        logger.debug(s"Found a filter on child table: $sel")

        val Generator(parSyms, _) = gPar
        //TODO cache attribution
        val filterSyms = new LeafSyms(new GraphTree(sel)).leafSyms(sel)

        if (filterSyms.intersect(parSyms.toSet).nonEmpty) {
          logger.debug("filter selector contains syms from both parent and child transformation")
          val mergedGen = Generator.merge(gChild, gFilter)
          (mergedGen, (gFilter, sel).some)
        } else {
          logger.debug("filter selector does not contain syms from both parent and child transformation")
          (gChild, None)
        }
      }) getOrElse((gChild, None))

      val ((update, rightTable), generator) = Generator.fuse(parTable, gPar, childTable, gMerged)
      val join = Join((gPar, parSym), (gChild.update(update), rightTable),
        if (filter.isEmpty) CartesianJoin else InnerJoin,
        filter.map { case (gen, f) => (gen.update(update), f)})

      sym.replaceWith(Transform(generator, join, select))
  }


  def buildComprehensions: Expression => Option[Expression] = {
    def buildComprehensionsImpl: Expression ==> Atom = {
      case Some(sym) /> _ if ComprehensionBuilder.comprehension(sym).nonEmpty =>
        ComprehensionBuilder.comprehension(sym).get
    }

    repeat(oncetd(rule[Expression](buildComprehensionsImpl))) andThen (_.map(_.asInstanceOf[Expression]))
  }
}
