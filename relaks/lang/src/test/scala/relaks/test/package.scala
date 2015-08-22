package relaks

import breeze.linalg
import breeze.linalg.DenseMatrix
/**
 * Created by Pietras on 22.08.15.
 */
package object test {
  def f1_accuracy(confMatrix: DenseMatrix[Double]): (Double, Double) = {
    import breeze.linalg._
    val truePositives = diag(confMatrix)
    val totalPredicted = sum(confMatrix(::, *)).t
    val falsePositives = totalPredicted :- truePositives
    val totalClass = sum(confMatrix(*, ::))
    val falseNegatives = totalClass :- truePositives

    val fptp = falsePositives :+ truePositives
    var nzeros = where(fptp :!= 0.0)
    val precision = DenseVector.zeros[Double](truePositives.length)
    precision(nzeros) := truePositives(nzeros) :/ fptp(nzeros)

    val tpfn = truePositives :+ falseNegatives
    nzeros = where(tpfn :!= 0.0)
    val recall = DenseVector.zeros[Double](truePositives.length)
    recall(nzeros) := truePositives(nzeros) :/ tpfn(nzeros)

    val accuracy = truePositives :/ totalPredicted

    val f1 = DenseVector.zeros[Double](truePositives.length)
    val pr = precision :+ recall
    nzeros = where(pr :!= 0.0)
    val pr2 = precision :* recall * 2.0
    f1(nzeros) := pr2(nzeros) :/ pr(nzeros)

    val support = totalClass / sum(totalClass)

    val weightedf1 = sum(f1 :* support)
    val weightedAcc = sum(accuracy :* support)


    (weightedf1, weightedAcc)
  }
}
