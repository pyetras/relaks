package relaks.lang.dsl.utils

import relaks.lang.dsl.AST._
import org.kiama.attribution.Attribution._
/**
 * Created by Pietras on 20/05/15.
 */
object TreeUtils {
  implicit class TreeOps(val tree: TTree) {
    def assertInitialized(): Unit = assert(tree.index != -1)
  }
}