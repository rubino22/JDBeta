/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.domains.numerical

import scala.util.Try
import it.unich.jandom.domains.CachedTopBottom
import it.unich.jandom.utils.breeze.countNonZero
import breeze.linalg._
import it.unich.jandom.utils.numberext.ModRationalGmpExt
import it.unich.jandom.utils.numberext.ModRationalGmpExt

/**
 * This is the abstract domain of parallelotopes as appears in the NSAD 2012 paper. It is written
 * using the Breeze Math library. It is not safe, due to rounding problem of arithmetic.
 *
 * @author Marco Rubino 
 */
class ParallelotopeDomainModRationalGmpExt private (favorAxes: Boolean) extends NumericalDomain {

  import breeze.linalg.operators._
  /**
   * Build a non-empty parallelotope. If the parallelotope is not empty, the result is undetermined.
   * @param A is the constraint matrix. It should be invertible.
   * @param low lower bounds.
   * @param high higher bounds.
   * @note `low` and `high` should have the same length. The  matrix `A` should be invertiible
   * of the same size of the two vectors.
   * @throws IllegalArgumentException if `low` and `high` are not of the same length, if `A` is not
   * square or if `A` has not the same size of `low`.
   */

  def apply(low: DenseVector[Double], A: DenseMatrix[Double], high: DenseVector[Double]): Property = {
   
    val low1= DenseVector.zeros[ModRationalGmpExt](low.length)
    val A1= DenseMatrix.zeros[ModRationalGmpExt](A.rows, A.cols)
    val high1= DenseVector.zeros[ModRationalGmpExt](high.length)
    for(i <- 0 until low.length){ low1(i)=ModRationalGmpExt(low(i))}    
    for(i <- 0 until high.length){ high1(i)=ModRationalGmpExt(high(i))}    
    for(i <- 0 until A.rows){ for(j <- 0 until A.cols){A1(i,j)=ModRationalGmpExt(A(i,j))}}
    
    val isEmpty = (0 until low1.size) exists { i => low1(i) > high1(i) }
    val isEmpty2 = (0 until low1.size) exists { i => low1(i).isInfinite && low1(i) == high1(i) }
  
    new Property(isEmpty || isEmpty2, low1, A1, high1)
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @throws $ILLEGAL
   */
  def top(n: Int): Property = {
  
    val low = DenseVector.fill(n)(ModRationalGmpExt.NegativeInfinity)
    val high = DenseVector.fill(n)(ModRationalGmpExt.PositiveInfinity)
    val A = DenseMatrix.eye[ModRationalGmpExt](n)
    
    new Property(false, low, A, high)
    /* The full parallelotope of dimension 0 is not empty! */
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @throws $ILLEGAL
   */
  def bottom(n: Int): Property = {
    val low = DenseVector.fill(n)(ModRationalGmpExt.one)
    val high = DenseVector.fill(n)(ModRationalGmpExt.zero)
    val A = DenseMatrix.eye[ModRationalGmpExt](n)    
    new Property(true, low, A, high)
  }

  /**
   * Given the box specified by `low` and `high` and the linear form `lf`, determines the pair
   * of the least and greatest value of the linear form in the box.
   * @param lf  a linear form.
   * @param low lower bound of the box.
   * @param high higher bound of the box.
   * @note `low`, `high` and `lf` should be of the same length.
   * @return the least and greatest value of `lf` in the box determine by `low` and `high`.
   */
  private def extremalsInBox(lf: DenseVector[ModRationalGmpExt], low: DenseVector[ModRationalGmpExt], high: DenseVector[ModRationalGmpExt]): (ModRationalGmpExt, ModRationalGmpExt) = {
    var minc = ModRationalGmpExt.zero
    var maxc = ModRationalGmpExt.zero
    for (i <- 0 until lf.length)
      if (lf(i) > ModRationalGmpExt.zero) {
        minc += lf(i) * low(i)
        maxc += lf(i) * high(i)
      } else if (lf(i) < ModRationalGmpExt.zero) {
        minc += lf(i) * high(i)
        maxc += lf(i) * low(i)
      }
    (minc, maxc)
  }

  /**
   * Given a sequence of vectors of the same length `n`, returns a sequence of `n` indexes
   * of vectors which are linearly independent. It is based on Gaussian elimination.
   * @param m a sequence of vectors, all of the same length.
   * @return a sequence of positions in m.
   */
  private def pivoting(m: IndexedSeq[DenseVector[ModRationalGmpExt]]): Seq[Int] = {
    val dimension = m(0).length
    var indexes = Seq[Int]()
    var pivots = Seq[(DenseVector[ModRationalGmpExt], Int)]()
    var i = 0
    while (indexes.length < dimension) {
      val row = m(i).copy
      for (p <- pivots) row -= p._1 * row(p._2)
      val col = (0 until row.length) find (row(_) != ModRationalGmpExt.zero)
      col match {
        case Some(col) =>
          row /= row(col)
          pivots = pivots :+ Tuple2(row, col)
          indexes = indexes :+ i
        case None =>
      }
      i += 1
    }
    indexes
  }

  /**
   * This is an element of the parallelotope domain.
   *
   * @constructor Builds a parallelotope
   * @param isEmpty is true if the parallelotope is empty. In this case, the other parameters are not relevant.
   * @param A is the constraint matrix. It should be invertible.
   * @param low lower bounds.
   * @param high higher bounds.
   * @note `low` and `high` should have the same length. The  matrix `A` should be invertible
   * of the same size of the two vectors.
   * @throws IllegalArgumentException if `low` and `high` are not of the same length, if `A` is not
   * square or if `A` has not the same size of `low`.
   */
  final class Property(
    val isEmpty: Boolean,
    val low: DenseVector[ModRationalGmpExt],
    val A: DenseMatrix[ModRationalGmpExt],
    val high: DenseVector[ModRationalGmpExt])
    extends NumericalProperty[Property] {
     
   require(low.length == A.rows)
    
   require(low.length == A.cols)
   
   println("A "+A+"  "+"      LOW "+low+"     HIGH   "+high)
   println("--------");
   
    require(Try(A \ DenseMatrix.eye[ModRationalGmpExt](dimension)).isSuccess, s"The shape matrix ${A} is not invertible")     
    require(normalized)
    //print("Low "+low+" --High ");
  // println(" "+high+" ===>"+isEmpty)
    type Domain = ParallelotopeDomainModRationalGmpExt

    def domain = ParallelotopeDomainModRationalGmpExt.this
 
    private def normalized: Boolean = {
     // println("low"+low+" high"+high);
     ( isEmpty || (low.length == high.length && (
        (0 until low.length - 1) forall {  i =>
          !low(i).isPosInfinity &&
            !high(i).isNegInfinity &&
            (low(i) <= high(i))
        })))
    }

    /**
     * @inheritdoc
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def widening(that: Property): Property = {
      require(dimension == that.dimension)
      if (isEmpty) return that
      val thatRotated = that.rotate(A)
      val newlow = low.copy
      val newhigh = high.copy
      for (i <- 0 to dimension - 1) {
        if (thatRotated.low(i) < low(i)) newlow(i) = ModRationalGmpExt.NegativeInfinity
        if (thatRotated.high(i) > high(i)) newhigh(i) = ModRationalGmpExt.PositiveInfinity
      }
      new Property(false, newlow, A, newhigh)
    }

    /**
     * @inheritdoc
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def narrowing(that: Property): Property = {
      require(dimension == that.dimension)
      if (that.isEmpty) {
        that
      } else {
        val thatRotated = that.rotate(A)
        val newlow = low.copy
        val newhigh = high.copy
        for (i <- 0 to dimension - 1) {
          if (low(i).isInfinity) newlow(i) = thatRotated.low(i) else newlow(i) = newlow(i) min thatRotated.low(i)
          if (high(i).isInfinity) newhigh(i) = thatRotated.high(i) else newhigh(i) = newhigh(i) max thatRotated.high(i)
        }
        new Property(false, newlow, A, newhigh)
      }
    }

    /**
     * @inheritdoc
     * It is equivalent to `intersectionWeak`.
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def intersection(that: Property): Property = intersectionWeak(that)

    /**
     * @inheritdoc
     * The union of two parallelotopes is not a parallelotope. Moreover, there is no least
     * parallelotopes containing the union of two parallelotopes. Hence, this methods uses
     * heuristics to find a good result.
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def union(that: Property): Property = {

      /*
       * A PrioritizedConstraint is a tuple `(a, m, M, p)` where `a` is a vector, `m` and `M` are
       * reals and `p` is an integer, whose intended meaning is the constraint `m <= ax <= M`
       * with a given priority `p`.
       */
      type PrioritizedConstraint = (DenseVector[ModRationalGmpExt], ModRationalGmpExt, ModRationalGmpExt, Int)

      /**
       * Given a linear form `v`, compute a prioritized constraint `(v,m,M,p)`. The parameter `ownedBy`
       * tells whether the line form under consideration is one of the "native" forms of this (1) or
       * that (2). This is used to refine priorities.
       */
      def priority(v: DenseVector[ModRationalGmpExt], ownedBy: Int = 0): PrioritizedConstraint = {
        val y1 = A.t \ v
        val (l1, u1) = domain.extremalsInBox(y1, low, high)
        val y2 = that.A.t \ v
        val (l2, u2) = domain.extremalsInBox(y2, that.low, that.high)
        val p =
          if (l1 == l2 && l2 == u1 && u1 == u2)
            0
          else if (favorAxes && countNonZero(v) == 1)
              25 //  previous value for test: 10
          else if (!l1.isInfinity && !l2.isInfinity && !u1.isInfinity && !u2.isInfinity) {
            if (l1 == l2 && u1 == u2)
              10
            else if (l1 >= l2 && u1 <= u2)
              if (ownedBy == 2) 20 else 30
            else if (l2 >= l1 && u2 <= u1)
              if (ownedBy == 1) 20 else 30
            else if (l2 <= u1 && l2 >= l1 && u2 >= u1)
              30
            else if (l2 <= l1 && u2 >= l1 && u2 <= u1)
              30
            else 40
          } else if (l1 == l2 && !l1.isInfinity && !l2.isInfinity)
            50
          else if (u1 == u2 && !u1.isInfinity && !u2.isInfinity)
            50
          else if (!l1.isInfinity && !l2.isInfinity)
            60
          else if (!u1.isInfinity && !u2.isInfinity)
            60
          else 100
        (v, l1 min l2, u1 max u2, p)
      }

      /**
       * Determines whether `v1` and `v2` are linearly dependent.
       * @return `None` if `v1` and `v2` are not linearly dependent, otherwise it is
       * `Some(k)` such that `v1 = k * v2`.
       */
      def linearDep(v1: DenseVector[ModRationalGmpExt], v2: DenseVector[ModRationalGmpExt]): Option[ModRationalGmpExt] = {
        var i: Int = 0
        while (i < dimension && (v1(i) == ModRationalGmpExt.zero || v2(i) == ModRationalGmpExt.zero)) i += 1
        if (i == dimension)
          Some(ModRationalGmpExt.one)
        else if (v1 / v1(i) == v2 / v2(i))
          Some(v1(i) / v2(i))
        else
          None
      }

      /**
       * The inversion join procedure.
       * @param vi first vector
       * @param vj second vector
       * @param min1i least value of v1 in this
       * @param min2i least value of v1 in that
       * @param min1j least value of v2 in that
       * @param min2j least value of v2 in that
       * @return None if `v1` and `v2` do not form an inversion, otherwise it is `Some(v)` where `v` is the
       * new linear form computed by the inversion procedure.
       */
      def newConstraint(vi: DenseVector[ModRationalGmpExt], vj: DenseVector[ModRationalGmpExt], min1i: ModRationalGmpExt, min2i: ModRationalGmpExt, min1j: ModRationalGmpExt, min2j: ModRationalGmpExt): Option[DenseVector[ModRationalGmpExt]] = {
        if (min1i.isInfinity || min2i.isInfinity || min1j.isInfinity || min2j.isInfinity) return None
        if (linearDep(vi, vj).isEmpty) return None
        val (deltai, deltaj) = if (min2j - min1j >= ModRationalGmpExt.zero) (min1i - min2i, min2j - min1j) else (min2i - min1i, min1j - min2j)
        if (deltai * deltaj > ModRationalGmpExt.zero)
          Some(-vi * deltaj - vj * deltai)
        else
          None
      }

      require(dimension == that.dimension)

      // special cases
      if (isEmpty) return that
      if (that.isEmpty) return this
      if (dimension == 0) return this;

      val thisRotated = this.rotate(that.A)
      val thatRotated = that.rotate(this.A)
      val Q = scala.collection.mutable.ArrayBuffer[PrioritizedConstraint]()

      val bulk = DenseMatrix.vertcat(this.A, that.A)
      val min1 = DenseVector.vertcat(this.low, thisRotated.low)
      val min2 = DenseVector.vertcat(thatRotated.low, that.low)
      val max1 = DenseVector.vertcat(this.high, thisRotated.high)
      val max2 = DenseVector.vertcat(thatRotated.high, that.high)

      for (i <- 0 to dimension - 1) Q += priority(this.A.t(::, i), 1)
      for (i <- 0 to dimension - 1) Q += priority(that.A.t(::, i), 2)
      for (i <- 0 to dimension - 1; j <- i + 1 to dimension - 1) {
        val v1 = bulk.t(::, i)
        val v2 = bulk.t(::, j)
        val nc1 = newConstraint(v1, v2, min1(i), min2(i), min1(j), min2(j))
        if (nc1.isDefined) Q += priority(nc1.get)
        val nc2 = newConstraint(v1, -v2, min1(i), min2(i), -max1(j), -max2(j))
        if (nc2.isDefined) Q += priority(nc2.get)
        val nc3 = newConstraint(-v1, -v2, -max1(i), -max2(i), -max1(j), -max2(j))
        if (nc3.isDefined) Q += priority(nc3.get)
        val nc4 = newConstraint(-v1, v2, -max1(i), -max2(i), min1(j), min2(j))
        if (nc4.isDefined) Q += priority(nc4.get)
      }
      val Qsorted = Q.sortBy[Int](_._4)
      val pvt = domain.pivoting(Qsorted map (_._1))

      val newA = DenseMatrix(pvt map (Qsorted(_)._1.toArray): _*)
      val newlow = DenseVector(pvt map (Qsorted(_)._2): _*)
      val newhigh = DenseVector(pvt map (Qsorted(_)._3): _*)
      
      new Property(false, newlow, newA, newhigh)
    }

    /**
     * This is a variant of `union` using weak join. The shape of the resulting
     * parallelotope is the same shap of `this`.
     * @param that the abstract object to be joined with `this`.
     * @note $NOTEDIMENSION
     * @return the weak union of the two abstract objects.
     */
    def unionWeak(that: Property): Property = {
      require(dimension == that.dimension)
      if (isEmpty) return that
      if (that.isEmpty) return this
      val result = that.rotate(A)
      for (i <- 0 to dimension - 1) {
        result.low(i) = result.low(i) min low(i)
        result.high(i) = result.high(i) max high(i)
      }
      new Property(false, result.low, result.A, result.high) //this is to normalize
    }

    /**
     * This is the weak intersection of two abstract objects. The shape of the resulting
     * parallelotope is the same shap of `this`.
     * @param that the abstract object to be intersected with `this`.
     * @note $NOTEDIMENSION
     * @return the intersection of the two abstract objects.
     */
    def intersectionWeak(that: Property): Property = {
      require(dimension == that.dimension)
      if (isEmpty) return this
      if (that.isEmpty) return that
      val result = that.rotate(A)
      for (i <- 0 to dimension - 1) {
        result.low(i) = result.low(i) max low(i)
        result.high(i) = result.high(i) min high(i)
      }
      if ((0 until result.low.length) exists { i => (result.low(i) > result.high(i)) })
        bottom
      else new Property(false, result.low, result.A, result.high) //this is to normalize
    }

    def linearAssignment(n: Int, lf: LinearForm[Double]): Property = linearAssignment_m(n, LinearForm(lf.coeffs map { ModRationalGmpExt(_) }: _*) )

    /**
     * @inheritdoc
     * @note @inheritdoc
     * @todo @inheritdoc
     * @throws $ILLEGAL
     */
    def linearAssignment_m(n: Int, lf: LinearForm[ModRationalGmpExt]): Property = {
      require(n <= dimension && lf.dimension <= dimension)
      val tcoeff = lf.homcoeffs
      val known = lf.known
      if (isEmpty) return this
      val coeff = tcoeff.padTo(dimension, ModRationalGmpExt.zero).toArray
      if (coeff(n) != ModRationalGmpExt.zero) {
        // invertible assignment
        val increment = A(::, n) :* known / coeff(n)
        val newlow = low :+ increment
        val newhigh = high :+ increment
        // in the past we could use SparseVector instead of DenseVector, but this does not work anymore
        val ei = DenseVector.zeros[ModRationalGmpExt](dimension)
        ei(n) = ModRationalGmpExt.one
        val newA = A :- (A(::, n) * (DenseVector(coeff) - ei).t) / coeff(n)
        new Property(false, newlow, newA, newhigh)
      } else {
        // non-invertible assignment
        val newP = nonDeterministicAssignment(n)
        val Aprime = newP.A.copy
        val j = ((0 to Aprime.rows - 1) find { Aprime(_, n) != ModRationalGmpExt.zero }).get
        for (s <- 0 to dimension - 1 if Aprime(s, n) != ModRationalGmpExt.zero && s != j)
          Aprime(s, ::) :-= Aprime(j, ::) * Aprime(s, n) / Aprime(j, n)
        val ei = DenseVector.zeros[ModRationalGmpExt](dimension)
        ei(n) = ModRationalGmpExt.one
        Aprime(j, ::) := (ei :- DenseVector(coeff)).t
        val newlow = newP.low.copy
        val newhigh = newP.high.copy
        newlow(j) = known
        newhigh(j) = known
        new Property(false, newlow, Aprime, newhigh)
      }
    }

    private def dotprod(x: DenseVector[ModRationalGmpExt], y: DenseVector[ModRationalGmpExt], remove: Int = -1): ModRationalGmpExt = {
      var sum= ModRationalGmpExt.zero
      for (i <- 0 until x.length if i != remove if x(i) != ModRationalGmpExt.zero) sum = sum + x(i) * y(i)
      sum
    }

    def linearInequality(lf: LinearForm[Double]): Property = linearInequality_m( LinearForm(lf.coeffs map { ModRationalGmpExt(_) }: _*) )

    /**
     * @inheritdoc
     * @note @inheritdoc
     * @todo @inheritdoc
     * @throws ILLEGAL
     */
    def linearInequality_m(lf: LinearForm[ModRationalGmpExt]): Property = {
      require(lf.dimension <= dimension)
      if (isEmpty) return this
      if (dimension == 0)
        if (lf.known > ModRationalGmpExt.zero)
          return bottom
        else
          return this

      val known = lf.known
      val coeffs = DenseVector(lf.homcoeffs.padTo(dimension, ModRationalGmpExt.zero): _*)
      val coeffsTransformed = A.t \ coeffs

      val removeCandidates = (0 until dimension) find { i => coeffsTransformed(i) != ModRationalGmpExt.zero && low(i).isInfinity && high(i).isInfinity }
      removeCandidates match {
        case None => {
          val newlow = low.copy
          val newhigh = high.copy
          val (minc, maxc) = domain.extremalsInBox(coeffsTransformed, newlow, newhigh)
          if (minc > -known) return bottom

          val lfArgmin = coeffsTransformed mapPairs { case (i, c) => if (c > ModRationalGmpExt.zero) low(i) else high(i) }

          val infinities = (0 until dimension) filter { i => lfArgmin(i).isInfinity && coeffsTransformed(i) != ModRationalGmpExt.zero }         
         println(infinities);
          infinities.size match {
            case 0 => 
              for (i <- 0 until dimension) {
                if (coeffsTransformed(i) > ModRationalGmpExt.zero) newhigh(i) = high(i) min (lfArgmin(i) + (-known - minc) / coeffsTransformed(i))
                else if (coeffsTransformed(i) < ModRationalGmpExt.zero) newlow(i) = low(i) max (lfArgmin(i) + (-known - minc) / coeffsTransformed(i))
              }
            case 1 => { 
              val posinf = infinities.head
              if (coeffsTransformed(posinf) < ModRationalGmpExt.zero)
                newlow(posinf) = low(posinf) max ((-dotprod(coeffsTransformed, lfArgmin, posinf) - known) / coeffsTransformed(posinf))
              else
                newhigh(posinf) = high(posinf) min ((-dotprod(coeffsTransformed, lfArgmin, posinf) - known) / coeffsTransformed(posinf))
            }
            case _ =>
          }
          new Property(false, newlow, A, newhigh)
        }
        case Some(chosen) => {
          // TODO: check.. I think this may generate non-invertible matrices
         
          val newA = A.copy
           println("A"+A);
          val newhigh = high.copy
          newA(chosen, ::) := coeffs.t
          newhigh(chosen) = -known
           println("A"+A);
           println("NewA"+newA);
          new Property(false, low, newA, newhigh)
        }
      }
    }

    def linearDisequality(lf: LinearForm[Double]): Property = linearDisequality_m( LinearForm(lf.coeffs map { ModRationalGmpExt(_) }: _*) )
    /**
     * @inheritdoc
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def linearDisequality_m(lf: LinearForm[ModRationalGmpExt]): Property = {     
      val tcoeff = lf.homcoeffs     
      val known = lf.known   
       
      if (tcoeff.forall(_ == ModRationalGmpExt.zero))
        if (known == ModRationalGmpExt.zero) bottom else this
      else {       
       
        val row = (0 until dimension).find { (r) =>
          val v1 = A(r, ::).t
          val v2 = DenseVector[ModRationalGmpExt](tcoeff: _*)
          v1 == v2
        }
        row match {
          case None => this
          case Some(row) =>
            if (low(row) == known && high(row) == known) bottom else this
        }
      }
    }

    /**
     * @inheritdoc
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def nonDeterministicAssignment(n: Int): Property = {
      require(n <= dimension)
      if (isEmpty) return this

      val unsortedCandidates = (0 until dimension) filter { i => A(i, n) != ModRationalGmpExt.zero && (!low(i).isNegInfinity || !high(i).isPosInfinity) }
      if (unsortedCandidates.isEmpty) return this

      // We prever to use as a pivot a simple constraint. Therefore, we order constraints by the number of
      // non-zero coefficients.
      val countNonZeroInRows = countNonZero(A(*, ::))
      val removeCandidates = unsortedCandidates.sortBy({ i => countNonZeroInRows(i) })
      val removeCandidatesEq = removeCandidates filter { i => low(i) == high(i) }
     
      val removeCandidatesBounded = removeCandidates filter { i => !low(i).isInfinity && !high(i).isInfinity }

      val pivot =
        if (!removeCandidatesEq.isEmpty) removeCandidatesEq.head
        else if (!removeCandidatesBounded.isEmpty) removeCandidatesBounded.head
        else removeCandidates.head
      val rowPivot = A(pivot, ::)

      val newA = A.copy
      val newlow = low.copy
      val newhigh = high.copy

      for (i <- removeCandidates if i != pivot) {
        val value1 = rowPivot(n)
        val value2 = A(i, n)
        val rowi = A(i, ::)
        newA(i, ::) := rowPivot * value2 - rowi * value1
        val (minPivot, maxPivot) = if (A(i, n) < ModRationalGmpExt.zero) (high(pivot), low(pivot)) else (low(pivot), high(pivot))
        val (mini, maxi) = if (-A(pivot, n) < ModRationalGmpExt.zero) (high(i), low(i)) else (low(i), high(i))
        newlow(i) = minPivot * value2 - mini * value1
        newhigh(i) = maxPivot * value2 - maxi * value1
      }
      newlow(pivot) = ModRationalGmpExt.NegativeInfinity
      newhigh(pivot) = ModRationalGmpExt.PositiveInfinity
      new Property(false, newlow, newA, newhigh)
    }

    def addVariable(): Property = {
      if (isEmpty)
        ParallelotopeDomainModRationalGmpExt.this.bottom(A.rows + 1)
      else {
        val e = DenseMatrix.zeros[ModRationalGmpExt](dimension + 1, 1)
        e(dimension, 0) = ModRationalGmpExt.one
        val newA = DenseMatrix.horzcat(DenseMatrix.vertcat(A, DenseMatrix.zeros[ModRationalGmpExt](1, dimension)), e)
        val newlow = DenseVector.vertcat(low, DenseVector(ModRationalGmpExt.NegativeInfinity))
        val newhigh = DenseVector.vertcat(high, DenseVector(ModRationalGmpExt.PositiveInfinity))
        new Property(false, newlow, newA, newhigh)
      }
    }

    def constraints = {
      if (isEmpty)
        Seq(LinearForm(1))
      else {
        val set1 = for (i <- 0 until dimension; if !low(i).isInfinity) yield -LinearForm(-low(i) +: A(i, ::).t.toScalaVector: _*)
        val set2 = for (i <- 0 until dimension; if !high(i).isInfinity) yield LinearForm(-high(i) +: A(i, ::).t.toScalaVector: _*)
        (set1 ++ set2) map { _.toDouble }
      }
    }

    def isPolyhedral = true

    /**
     * @inheritdoc
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def delVariable(n: Int): Property = {
      def rowToSeq(M: DenseMatrix[ModRationalGmpExt], i: Int, n: Int): Seq[ModRationalGmpExt] =
        for (j <- 0 until A.rows; if j != n) yield M(i, j)

      if (isEmpty)
        ParallelotopeDomainModRationalGmpExt.this.bottom(A.rows - 1)
      else {
        val forgot = this.nonDeterministicAssignment(n)
        val set1 = for (i <- 0 until dimension; if !forgot.low(i).isInfinity; if forgot.A(i, n) == ModRationalGmpExt.zero) yield -LinearForm(-forgot.low(i) +: rowToSeq(forgot.A, i, n): _*)
        val set2 = for (i <- 0 until dimension; if !forgot.high(i).isInfinity; if forgot.A(i, n) == ModRationalGmpExt.zero) yield LinearForm(-forgot.high(i) +: rowToSeq(forgot.A, i, n): _*)
        (set1 ++ set2).foldLeft(ParallelotopeDomainModRationalGmpExt.this.top(A.rows - 1)) { (p, lf) => p.linearInequality_m(lf) }
      }
    }

    /**
     * @inheritdoc
     */
    def mapVariables(rho: Seq[Int]): Property = {
      if (isEmpty)
        this
      else {
        val slice = for (i <- 0 until dimension; j = rho.indexOf(i); if j != -1) yield j
        val newA = A(slice, slice).toDenseMatrix
        val newlow = low(slice).toDenseVector
        val newhigh = high(slice).toDenseVector
        new Property(false, newlow, newA, newhigh)
      }
    }

     def linearEvaluation(lf: LinearForm[Double]): (Double, Double) = {
       val le =linearEvaluation_m(LinearForm(lf.coeffs map { ModRationalGmpExt(_) }: _*))
       (le._1.toDouble, le._2.toDouble)
     }

    /**
     * Compute the minimum and maximum value of a linear form in a parallelotope.
     * @todo should be generalized to linear forms over arbitrary types.
     * @return a tuple with two components: the first component is the least value, the second component is the greatest value
     * of the linear form over the box.
     */
    def linearEvaluation_m(lf: LinearForm[ModRationalGmpExt]): (ModRationalGmpExt, ModRationalGmpExt) = {

      val tcoeff = lf.homcoeffs
      if (isEmpty && tcoeff.exists { _ != ModRationalGmpExt.zero })
        (ModRationalGmpExt.PositiveInfinity,ModRationalGmpExt.NegativeInfinity)
      else if (dimension == 0)
        (lf.known, lf.known)
      else {
        val coeff = tcoeff.padTo(dimension, ModRationalGmpExt.zero).toArray
        val vec = DenseVector(coeff)
        val newvec = A.t \ vec
        val newlf = lf.known +: newvec.valuesIterator.toSeq
        val (min, max) = domain.extremalsInBox(newvec, low, high)
        (min + lf.known, max + lf.known)
      }
    }

    def minimize_m(lf: LinearForm[ModRationalGmpExt]) = linearEvaluation_m(lf)._1

    def maximize_m(lf: LinearForm[ModRationalGmpExt]) = linearEvaluation_m(lf)._2

    def frequency_m(lf: LinearForm[ModRationalGmpExt]) = {
      val (min, max) = linearEvaluation_m(lf)
      if (min == max) Some(min) else None
    }
    
    
    def minimize(lf: LinearForm[Double]) =  linearEvaluation(lf)._1

    def maximize(lf: LinearForm[Double]) = linearEvaluation(lf)._2

    def frequency(lf: LinearForm[Double]) = {
      val (min, max) = linearEvaluation(lf)
      if (min == max) Some(min) else None
    }

    def dimension = A.rows

    def isTop = low.forall(_.isNegInfinity) && high.forall(_.isPosInfinity)

    def isBottom = isEmpty

    def bottom = ParallelotopeDomainModRationalGmpExt.this.bottom(dimension)

    def top = ParallelotopeDomainModRationalGmpExt.this.top(dimension)

    def tryCompareTo[B >: Property](that: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = that match {
      case that: Property =>
        val lte = this <= that
        val gte = that <= this
        if (lte && gte)
          Some(0)
        else if (lte)
          Some(-1)
        else if (gte)
          Some(1)
        else
          None
      case _ => None
    }

    /**
     * It computes the smallest parallelotope which contains `this` and is definable
     * over a new shape matrix `Aprime`.
     * @param Aprime the new shape matrix.
     * @note `Aprime` should be an invertible matrix of the same dimension as `this`.
     * @throws IllegalArgumentException if `Aprime` is not square or has not the correct dimension.
     */
    def rotate(Aprime: DenseMatrix[ModRationalGmpExt]): Property = {
      require(dimension == Aprime.rows && dimension == Aprime.cols)
      if (isEmpty) return this;
      val B = Aprime * (A \ DenseMatrix.eye[ModRationalGmpExt](dimension))
      val newlow = DenseVector.zeros[ModRationalGmpExt](dimension)
      val newhigh = DenseVector.zeros[ModRationalGmpExt](dimension)
      B.foreachPair {
        case ((i, j), v) =>
          if (v > ModRationalGmpExt.zero) {
            newlow(i) += v * low(j)
            newhigh(i) += v * high(j)
          } else if (v < ModRationalGmpExt.zero) {
            newhigh(i) += v * low(j)
            newlow(i) += v * high(j)
          }
      }
      new Property(false, newlow, Aprime, newhigh)
    }

    def <=[B >: Property](that: Property)(implicit arg0: (B) => PartiallyOrdered[B]): Boolean = {
      if (isEmpty) return (true)
      if (that.isEmpty) return (false)
      if (that.isTop) return (true)
      val ptemp = this.rotate(that.A)
      (0 to ptemp.low.length - 1) forall { i => ptemp.low(i) >= that.low(i) && ptemp.high(i) <= that.high(i) }
    }

    def >=[B >: Property](that: Property)(implicit arg0: (B) => PartiallyOrdered[B]): Boolean =
      that <= this

    def <[B >: Property](that: Property)(implicit arg0: (B) => PartiallyOrdered[B]): Boolean =
      (this <= that) && !(this >= that)

    def >[B >: Property](that: Property)(implicit arg0: (B) => PartiallyOrdered[B]): Boolean =
      (this >= that) && !(this <= that)

    def mkString(vars: Seq[String]): String = {

      /**
       * Returns a string representation of the linear form `lf`.
       */
      def lfToString(lf: DenseVector[ModRationalGmpExt]): String = {
        var first = true
        var s = ""

        for (index <- 0 until dimension) {
          val coeff = lf(index)
          val term = coeff match {
            case ModRationalGmpExt.zero => ""
            case ModRationalGmpExt.one => vars(index)
            //case -1 => "-" + vars(index)
            case c => c.toString + "*" + vars(index)
          }
          if (coeff != ModRationalGmpExt.zero) {
            if (first || coeff < ModRationalGmpExt.zero) {
              s += term
              first = false
            } else if (coeff != ModRationalGmpExt.zero)
              s += "+" + term
          }
        }
        if (s.isEmpty) "0" else s
      }

      if (isEmpty)
        "empty"
      else {
        val eqns = for (i <- 0 until dimension) yield {
          if (low(i) < high(i))
            low(i) + " <= " + lfToString(A.t(::, i)) + " <= " + high(i)
          else lfToString(A.t(::, i)) + " = " + high(i)
        }
        eqns.mkString("[ ", " , ", " ]")
      }
    }
  }

}

/**
 * Companion class for the parallelotope domain
 */
object ParallelotopeDomainModRationalGmpExt {
  private lazy val standard = new ParallelotopeDomainModRationalGmpExt(false) with CachedTopBottom
  private lazy val favoring = new ParallelotopeDomainModRationalGmpExt(true) with CachedTopBottom

  /**
   * Returns an abstract domain for parallelotopes.
   * @param favorAxes determines whether the heuristics built into the domain should try to keep the variability
   * along axes at the minimum. This is generally useful when the domain is one of the component of the sum
   * combinator.
   */
  def apply(favorAxes: Boolean = false) = if (favorAxes) favoring else standard
}
