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
import it.unich.jandom.utils.numberext.ModRationalSpireExt
import it.unich.jandom.utils.numberext.ModRationalSpireExt
import it.unich.jandom.ui.Parameter
import it.unich.jandom.utils.numberext.ModRationalSpireExt
import scala.reflect.io.File
import scala.collection.mutable.ArrayBuffer
import it.unich.jandom.utils.numberext.ModRationalSpireExt.ModRationalSpireExtZero
import scala.collection.mutable.ListBuffer
import javafx.beans.binding.ListBinding



/**
 * This is the base class for rational extensions
 *
 * @author Marco Rubino <marco.rubino@unich.it>
 */
class ParallelotopeDomainModQSpire private (favorAxes: Integer, overRound: Boolean) extends NumericalDomain {

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
  var  count =0
  def apply(low: DenseVector[ModRationalSpireExt], A: DenseMatrix[ModRationalSpireExt], high: DenseVector[ModRationalSpireExt]): Property = {
   
   /* val low1= DenseVector.zeros[ModRationalSpireExt](low.length)
    val A1= DenseMatrix.zeros[ModRationalSpireExt](A.rows, A.cols)
    val high1= DenseVector.zeros[ModRationalSpireExt](high.length)
    for(i <- 0 until low.length){ low1(i)=ModRationalSpireExt(low(i))}    
    for(i <- 0 until high.length){ high1(i)=ModRationalSpireExt(high(i))}    
    for(i <- 0 until A.rows){ for(j <- 0 until A.cols){A1(i,j)=ModRationalSpireExt(A(i,j))}}
    */
     
    val isEmpty = (0 until low.size) exists { i => low(i) > high(i) }
    val isEmpty2 = (0 until low.size) exists { i => low(i).isInfinite && low(i) == high(i) }
    //println("Prop "+low+" "+high);
    new Property(isEmpty || isEmpty2, low, A, high)
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @throws $ILLEGAL
   */
  def top(n: Int): Property = {
  
    val low = DenseVector.fill(n)(ModRationalSpireExt.NegativeInfinity)
    val high = DenseVector.fill(n)(ModRationalSpireExt.PositiveInfinity)
    val A = DenseMatrix.eye[ModRationalSpireExt](n)
   
    new Property(false, low, A, high)
    /* The full parallelotope of dimension 0 is not empty! */
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @throws $ILLEGAL
   */
  def bottom(n: Int): Property = {
    val low = DenseVector.fill(n)(ModRationalSpireExt.one)
    val high = DenseVector.fill(n)(ModRationalSpireExt.zero)
    val A = DenseMatrix.eye[ModRationalSpireExt](n)  
   
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
  private def extremalsInBox(lf: DenseVector[ModRationalSpireExt], low: DenseVector[ModRationalSpireExt], high: DenseVector[ModRationalSpireExt]): (ModRationalSpireExt, ModRationalSpireExt) = {
    var minc = ModRationalSpireExt.zero
    var maxc = ModRationalSpireExt.zero
    for (i <- 0 until lf.length)
      if (lf(i) > ModRationalSpireExt.zero) {
        minc += lf(i) * low(i)
        maxc += lf(i) * high(i)
      } else if (lf(i) < ModRationalSpireExt.zero) {
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
  private def pivoting(m: IndexedSeq[DenseVector[ModRationalSpireExt]]): Seq[Int] = {
    val dimension = m(0).length
   
    var indexes = Seq[Int]()
   
    var pivots = Seq[(DenseVector[ModRationalSpireExt], Int)]()
    var i = 0
    while (indexes.length < dimension) {
 
    
      val row = m(i).copy
      for (p <- pivots) row -= p._1 * row(p._2)
      val col = (0 until row.length) find (row(_) != ModRationalSpireExt.zero)
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
    val low: DenseVector[ModRationalSpireExt],
    val A: DenseMatrix[ModRationalSpireExt],
    val high: DenseVector[ModRationalSpireExt])
    extends NumericalProperty[Property] {
    
   //require(low.length == A.rows)
    
   //require(low.length == A.cols)

   //println("--------");
  // println(A)
  // println("--------");
   
   // require(Try(A \ DenseMatrix.eye[ModRationalSpireExt](dimension)).isSuccess, s"The shape matrix ${A} is not invertible")     
   // require(normalized)
    //println("Low "+low+" --High "+high);
   
    type Domain = ParallelotopeDomainModQSpire

    def domain = ParallelotopeDomainModQSpire.this
 
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
   
   //   require(dimension == that.dimension)
   
      if (isEmpty) return that
      val thatRotated = that.rotate(A)
      val thisRotated = this.rotate(that.A) 
     
      if(thisRotated<that ){
   
        val newlow = thisRotated.low.copy
        val newhigh = thisRotated.high.copy
          for (i <- 0 to dimension - 1) {        
          if (thisRotated.low(i) > that.low(i)) newlow(i) = ModRationalSpireExt.NegativeInfinity
          if (thisRotated.high(i) < that.high(i)) newhigh(i) = ModRationalSpireExt.PositiveInfinity
        }
     
      return   new Property(false, newlow, that.A, newhigh)
        
      }else{

   
            val newlow = low.copy
            val newhigh = high.copy
            for (i <- 0 to dimension - 1) {
              if (thatRotated.low(i) < low(i)) newlow(i) = ModRationalSpireExt.NegativeInfinity
              if (thatRotated.high(i) > high(i)) newhigh(i) = ModRationalSpireExt.PositiveInfinity
      
              }         
        
             return  new Property(false, newlow, A, newhigh) 
      }      
    }

    
   
    
    
    /**
     * @inheritdoc
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def narrowing(that: Property): Property = {
     // require(dimension == that.dimension)
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
       * rational and `p` is an integer, whose intended meaning is the constraint `m <= ax <= M`
       * with a given priority `p`.
       */
      type PrioritizedConstraint = (DenseVector[ModRationalSpireExt], ModRationalSpireExt, ModRationalSpireExt, Int)

      /*
       * Given a linear form `v`, compute a prioritized constraint `(v,m,M,p)`. The parameter `ownedBy`
       * tells whether the line form under consideration is one of the "native" forms of this (1) or
       * that (2). This is used to refine priorities.
       */
      def priority(v: DenseVector[ModRationalSpireExt], ownedBy: Int = 0): PrioritizedConstraint = {
        var y1 = A.t \ v
        var (l1, u1) = domain.extremalsInBox(y1, low, high)
        var y2 = that.A.t \ v
        var (l2, u2) = domain.extremalsInBox(y2, that.low, that.high)
        
      /*  var p =
          if (l1 == l2 && l2 == u1 && u1 == u2) ///se appartiene a tutti e due è necessario
            0          
          else if (favorAxes == 1 && countNonZero(v) == 1) // se è un asse si prende
           1
           else if (favorAxes == -1 && countNonZero(v) == 1) // se è un asse si cerca di non prendere
           99
          else if (!l1.isInfinity && !l2.isInfinity && !u1.isInfinity && !u2.isInfinity) {// diversi criteri di priorità
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
          else
            100*/
         
        var p =
          if (favorAxes == 1 && countNonZero(v) == 1) // favorAxes
           0
           else if (favorAxes == -1 && countNonZero(v) == 1) // not favorAxes
           101
           else if (l1 == l2 && l2 == u1 && u1 == u2) /// if nothing choose axes
            1          
   
          else if (!l1.isInfinity && !l2.isInfinity && !u1.isInfinity && !u2.isInfinity) {// diversi criteri di priorità
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
          else
            100
        
        (v, l1 min l2, u1 max u2, p)
      }

      /*
       * Determines whether `v1` and `v2` are linearly dependent.
       * @return `None` if `v1` and `v2` are not linearly dependent, otherwise it is
       * `Some(k)` such that `v1 = k * v2`.
       */
      def linearDep(v1: DenseVector[ModRationalSpireExt], v2: DenseVector[ModRationalSpireExt]): Option[ModRationalSpireExt] = {
        var i: Int = 0
        while (i < dimension && (v1(i) == ModRationalSpireExt.zero || v2(i) == ModRationalSpireExt.zero)) i += 1
        if (i == dimension)
          Some(ModRationalSpireExt.one)
        else if (v1 / v1(i) == v2 / v2(i))
          Some(v1(i) / v2(i))
        else
          None
      }

      
    
        
        /*def test(Q1: PrioritizedConstraint, Q:scala.collection.mutable.ArrayBuffer[PrioritizedConstraint]) : PrioritizedConstraint ={
           var d=0;
          for(y<-0 until Q.length){
            
          if((Q(y)._1)==((Q1._1)) && (Q(y)._2)==((Q1._2))&& (Q(y)._3)==((Q1._3))&&((Q(y)._4)==((Q1._4))) && d!=1){
           d=1;
            //println("uguale "+y+" "+Q1+ " -->"+Q);
            //val Q2=PrioritizedConstraint()
            //Q2=((Q1._1),(Q1._2),(Q1._3),102);
          }
         
          }
          if(d==1){
          return ((Q1._1),(Q1._2),(Q1._3),104)
          }else
           Q1
        }*/
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
      def newConstraint(vi: DenseVector[ModRationalSpireExt], vj: DenseVector[ModRationalSpireExt], min1i: ModRationalSpireExt, min2i: ModRationalSpireExt, min1j: ModRationalSpireExt, min2j: ModRationalSpireExt): Option[DenseVector[ModRationalSpireExt]] = {
        if (min1i.isInfinity || min2i.isInfinity || min1j.isInfinity || min2j.isInfinity) return None
        if (linearDep(vi, vj).isEmpty) return None
        val (deltai, deltaj) = if (min2j - min1j >= ModRationalSpireExt.zero) (min1i - min2i, min2j - min1j) else (min2i - min1i, min1j - min2j)
        if (deltai * deltaj > ModRationalSpireExt.zero)
          Some(-vi * deltaj - vj * deltai)
        else
          None
      }

      //require(dimension == that.dimension)

      // special cases
      if (isEmpty) return that
      if (that.isEmpty) return this
      if (dimension == 0) return this;

      var thisRotated = this.rotate(that.A)
      var thatRotated = that.rotate(this.A)
      val Q = scala.collection.mutable.ArrayBuffer[PrioritizedConstraint]()
     
      
      var bulk = DenseMatrix.vertcat(this.A, that.A)
      var min1 = DenseVector.vertcat(this.low, thisRotated.low)
      var min2 = DenseVector.vertcat(thatRotated.low, that.low)
      var max1 = DenseVector.vertcat(this.high, thisRotated.high)
      var max2 = DenseVector.vertcat(thatRotated.high, that.high)      
      for (i <- 0 to dimension - 1) Q += priority(this.A.t(::, i), 1)
      for (i <- 0 to dimension - 1) Q += priority(that.A.t(::, i), 2)
      for (i <- 0 to dimension - 1; j <- i + 1 to dimension - 1) {
        var v1 = bulk.t(::, i)
        var v2 = bulk.t(::, j)
        
        var nc1 = newConstraint(v1, v2, min1(i), min2(i), min1(j), min2(j))
        if (nc1.isDefined) Q += priority(nc1.get)
        var nc2 = newConstraint(v1, -v2, min1(i), min2(i), -max1(j), -max2(j))
        if (nc2.isDefined) Q += priority(nc2.get)
        var nc3 = newConstraint(-v1, -v2, -max1(i), -max2(i), -max1(j), -max2(j))
        if (nc3.isDefined) Q += priority(nc3.get)
        var nc4 = newConstraint(-v1, v2, -max1(i), -max2(i), min1(j), min2(j))
        if (nc4.isDefined) Q += priority(nc4.get)
       
      }
      
     val Qsorted = Q.sortBy[Int](_._4)
    // println(Qsorted);
   /*   //println("NUOVO");
      val rem: ListBuffer[Int] =ListBuffer()
      QSingular=Q.distinct
     for(i<-0 to QSingular.length){
        for(j<-i+1 to QSingular.length-1){
      
          if((QSingular(i)._1)==((QSingular(j)._1)) && (QSingular(i)._2)==((QSingular(j)._2))&& (QSingular(i)._3)==((QSingular(j)._3))&&((QSingular(i)._4)==((QSingular(j)._4)))){
           //println("Uguali "+QSingular(i)+" "+QSingular(j));
           //QSingular+=QSingular(i)
            rem +=j
            
          //  println("remove "+j);
          }
        }
      }
     // println(" PRE ------ "+QSingular);
      for(n <-0 until rem.length){
    // println("removed "+rem(n)+" "+rem.length);
        QSingular.remove(rem(n)-n)
      //  println(QSingular)
      }
    //  println("  POST ------ "+QSingular);
      val Qsorted =QSingular.sortBy[Int](_._4)
      /*if(Q.length!=QSingular.length){
        println(Q+"  ------ "+QSingular);
      }*/
      //println("lunghezza "+Q.length +" "+QSingular.length);*/
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
    //  require(dimension == that.dimension)
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
    //  require(dimension == that.dimension)
   
      if (isEmpty) return this
      if (that.isEmpty) return that
      
      val result = that.rotate(A)
    
      for (i <- 0 to dimension - 1) {
        result.low(i) = result.low(i) max low(i)
        result.high(i) = result.high(i) min high(i)
      }
    
    if ((0 until result.low.length) exists { i => (result.low(i) > result.high(i)) })
        bottom
      else  new Property(false, result.low, result.A, result.high) //this is to normalize
    }

    def linearAssignment(n: Int, lf: LinearForm[Double]): Property = linearAssignment_m(n, LinearForm(lf.coeffs map { ModRationalSpireExt(_) }: _*) )

    /**
     * @inheritdoc
     * @note @inheritdoc
     * @todo @inheritdoc
     * @throws $ILLEGAL
     */
    def linearAssignment_m(n: Int, lf: LinearForm[ModRationalSpireExt]): Property = {
    //  require(n <= dimension && lf.dimension <= dimension)
      val tcoeff = lf.homcoeffs
      val known = lf.known
      if (isEmpty) return this
      val coeff = tcoeff.padTo(dimension, ModRationalSpireExt.zero).toArray
    
      if (coeff(n) != ModRationalSpireExt.zero) {
        // invertible assignment
        val increment = A(::, n) :* known / coeff(n)
        val newlow = low :+ increment
        val newhigh = high :+ increment
        // in the past we could use SparseVector instead of DenseVector, but this does not work anymore
        val ei = DenseVector.zeros[ModRationalSpireExt](dimension)
        ei(n) = ModRationalSpireExt.one
        val newA = A :- (A(::, n) * (DenseVector(coeff) - ei).t) / coeff(n)
     
        new Property(false, newlow, newA, newhigh)
      } else {
        // non-invertible assignment
        val newP = nonDeterministicAssignment(n)
        val Aprime = newP.A.copy
        val j = ((0 to Aprime.rows - 1) find { Aprime(_, n) != ModRationalSpireExt.zero }).get
        for (s <- 0 to dimension - 1 if Aprime(s, n) != ModRationalSpireExt.zero && s != j)
          Aprime(s, ::) :-= Aprime(j, ::) * Aprime(s, n) / Aprime(j, n)
        val ei = DenseVector.zeros[ModRationalSpireExt](dimension)
        ei(n) = ModRationalSpireExt.one
        Aprime(j, ::) := (ei :- DenseVector(coeff)).t
        val newlow = newP.low.copy
        val newhigh = newP.high.copy
        newlow(j) = known
        newhigh(j) = known
      
        new Property(false, newlow, Aprime, newhigh)
      }
    }

    private def dotprod(x: DenseVector[ModRationalSpireExt], y: DenseVector[ModRationalSpireExt], remove: Int = -1): ModRationalSpireExt = {
      var sum= ModRationalSpireExt.zero
      for (i <- 0 until x.length if i != remove if x(i) != ModRationalSpireExt.zero) sum = sum + x(i) * y(i)
      sum
    }

    def linearInequality(lf: LinearForm[Double]): Property = linearInequality_m( LinearForm(lf.coeffs map { ModRationalSpireExt(_) }: _*) )

    /**
     * @inheritdoc
     * @note @inheritdoc
     * @todo @inheritdoc
     * @throws ILLEGAL
     */
    def linearInequality_m(lf: LinearForm[ModRationalSpireExt]): Property = {
     // require(lf.dimension <= dimension)
      if (isEmpty) return this
      if (dimension == 0)
        if (lf.known > ModRationalSpireExt.zero)
          return bottom
        else
          return this

      val known = lf.known
      val coeffs = DenseVector(lf.homcoeffs.padTo(dimension, ModRationalSpireExt.zero): _*)
     
      val coeffsTransformed = A.t \ coeffs
     
      val removeCandidates = (0 until dimension) find { i => coeffsTransformed(i) != ModRationalSpireExt.zero && low(i).isInfinity && high(i).isInfinity }
     
      removeCandidates match {
        case None => {
          val newlow = low.copy
          val newhigh = high.copy
          val (minc, maxc) = domain.extremalsInBox(coeffsTransformed, newlow, newhigh)
          if (minc > -known) return bottom

          val lfArgmin = coeffsTransformed mapPairs { case (i, c) => if (c > ModRationalSpireExt.zero) low(i) else high(i) }

          val infinities = (0 until dimension) filter { i => lfArgmin(i).isInfinity && coeffsTransformed(i) != ModRationalSpireExt.zero }         
     
          infinities.size match {
            case 0 => 
              for (i <- 0 until dimension) {
     
                if (coeffsTransformed(i) > ModRationalSpireExt.zero) newhigh(i) = high(i) min (lfArgmin(i) + (-known - minc) / coeffsTransformed(i))
                else if (coeffsTransformed(i) < ModRationalSpireExt.zero) newlow(i) = low(i) max (lfArgmin(i) + (-known - minc) / coeffsTransformed(i))
     
                }
            case 1 => { 
              val posinf = infinities.head
              if (coeffsTransformed(posinf) < ModRationalSpireExt.zero)
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
           
          val newhigh = high.copy
          newA(chosen, ::) := coeffs.t
          newhigh(chosen) = -known
          
          new Property(false, low, newA, newhigh)
        }
      }
    }

    def linearDisequality(lf: LinearForm[Double]): Property = linearDisequality_m( LinearForm(lf.coeffs map { ModRationalSpireExt(_) }: _*) )
    /**
     * @inheritdoc
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def linearDisequality_m(lf: LinearForm[ModRationalSpireExt]): Property = {     
      val tcoeff = lf.homcoeffs     
      val known = lf.known   
       
      if (tcoeff.forall(_ == ModRationalSpireExt.zero))
        if (known == ModRationalSpireExt.zero) bottom else this
      else {       
       
        val row = (0 until dimension).find { (r) =>
          val v1 = A(r, ::).t
          val v2 = DenseVector[ModRationalSpireExt](tcoeff: _*)
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
     // require(n <= dimension)
      if (isEmpty) return this

      val unsortedCandidates = (0 until dimension) filter { i => A(i, n) != ModRationalSpireExt.zero && (!low(i).isNegInfinity || !high(i).isPosInfinity) }
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
        val (minPivot, maxPivot) = if (A(i, n) < ModRationalSpireExt.zero) (high(pivot), low(pivot)) else (low(pivot), high(pivot))
        val (mini, maxi) = if (-A(pivot, n) < ModRationalSpireExt.zero) (high(i), low(i)) else (low(i), high(i))
        newlow(i) = minPivot * value2 - mini * value1
        newhigh(i) = maxPivot * value2 - maxi * value1
      }
      newlow(pivot) = ModRationalSpireExt.NegativeInfinity
      newhigh(pivot) = ModRationalSpireExt.PositiveInfinity
      
      new Property(false, newlow, newA, newhigh)
    }

    def addVariable(): Property = {
      if (isEmpty)
        ParallelotopeDomainModQSpire.this.bottom(A.rows + 1)
      else {
        val e = DenseMatrix.zeros[ModRationalSpireExt](dimension + 1, 1)
        e(dimension, 0) = ModRationalSpireExt.one
        val newA = DenseMatrix.horzcat(DenseMatrix.vertcat(A, DenseMatrix.zeros[ModRationalSpireExt](1, dimension)), e)
        val newlow = DenseVector.vertcat(low, DenseVector(ModRationalSpireExt.NegativeInfinity))
        val newhigh = DenseVector.vertcat(high, DenseVector(ModRationalSpireExt.PositiveInfinity))
        
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
      def rowToSeq(M: DenseMatrix[ModRationalSpireExt], i: Int, n: Int): Seq[ModRationalSpireExt] =
        for (j <- 0 until A.rows; if j != n) yield M(i, j)

      if (isEmpty)
        ParallelotopeDomainModQSpire.this.bottom(A.rows - 1)
      else {
        val forgot = this.nonDeterministicAssignment(n)
        val set1 = for (i <- 0 until dimension; if !forgot.low(i).isInfinity; if forgot.A(i, n) == ModRationalSpireExt.zero) yield -LinearForm(-forgot.low(i) +: rowToSeq(forgot.A, i, n): _*)
        val set2 = for (i <- 0 until dimension; if !forgot.high(i).isInfinity; if forgot.A(i, n) == ModRationalSpireExt.zero) yield LinearForm(-forgot.high(i) +: rowToSeq(forgot.A, i, n): _*)
        (set1 ++ set2).foldLeft(ParallelotopeDomainModQSpire.this.top(A.rows - 1)) { (p, lf) => p.linearInequality_m(lf) }
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
       val le =linearEvaluation_m(LinearForm(lf.coeffs map { ModRationalSpireExt(_) }: _*))
       (le._1.toDouble, le._2.toDouble)
     }

    /**
     * Compute the minimum and maximum value of a linear form in a parallelotope.
     * @todo should be generalized to linear forms over arbitrary types.
     * @return a tuple with two components: the first component is the least value, the second component is the greatest value
     * of the linear form over the box.
     */
    def linearEvaluation_m(lf: LinearForm[ModRationalSpireExt]): (ModRationalSpireExt, ModRationalSpireExt) = {

      val tcoeff = lf.homcoeffs
      if (isEmpty && tcoeff.exists { _ != ModRationalSpireExt.zero })
        (ModRationalSpireExt.PositiveInfinity,ModRationalSpireExt.NegativeInfinity)
      else if (dimension == 0)
        (lf.known, lf.known)
      else {
        val coeff = tcoeff.padTo(dimension, ModRationalSpireExt.zero).toArray
        val vec = DenseVector(coeff)
        val newvec = A.t \ vec
        val newlf = lf.known +: newvec.valuesIterator.toSeq
        val (min, max) = domain.extremalsInBox(newvec, low, high)
        (min + lf.known, max + lf.known)
      }
    }

    def minimize_m(lf: LinearForm[ModRationalSpireExt]) = linearEvaluation_m(lf)._1

    def maximize_m(lf: LinearForm[ModRationalSpireExt]) = linearEvaluation_m(lf)._2

    def frequency_m(lf: LinearForm[ModRationalSpireExt]) = {
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

    def bottom = ParallelotopeDomainModQSpire.this.bottom(dimension)

    def top = ParallelotopeDomainModQSpire.this.top(dimension)

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
    def rotate(Aprime: DenseMatrix[ModRationalSpireExt]): Property = {
     //require(dimension == Aprime.rows && dimension == Aprime.cols)
      if (isEmpty) return this;
     
      val B = Aprime * (A \ DenseMatrix.eye[ModRationalSpireExt](dimension))
   
     
      val newlow = DenseVector.zeros[ModRationalSpireExt](dimension)
      val newhigh = DenseVector.zeros[ModRationalSpireExt](dimension)
      
        B.foreachPair {
        case ((i, j), v) =>
          
          if (v > ModRationalSpireExt.zero) {          
            newlow(i) += v * low(j)
            newhigh(i) += v * high(j)     
          } else if (v < ModRationalSpireExt.zero) {      
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
      def lfToString(lf: DenseVector[ModRationalSpireExt]): String = {
        var first = true
        var s = ""
        
        for (index <- 0 until dimension) {
          val coeff = lf(index)
          val term = coeff match {
            case ModRationalSpireExt.zero => ""
            case ModRationalSpireExt.one => vars(index)
            //case -1 => "-" + vars(index)
            case c => c.toString + "*" + vars(index)
          }
          if (coeff != ModRationalSpireExt.zero) {
            if (first || coeff < ModRationalSpireExt.zero) {
              s += term
              first = false
            } else if (coeff != ModRationalSpireExt.zero)
              s += "+" + term
          }
        }
        if (s.isEmpty) "0" else s
      }

      if (isEmpty)
        "empty"
      else {
        
        val eqns = for (i <- 0 until dimension) yield {
          if (low(i) < high(i)){
            if(overRound){
              
            low(i).rounds("Down") + " <= " + lfToString(A.t(::, i)) + " <= " + high(i).rounds("Up")
          }else{
            low(i) + " <= " + lfToString(A.t(::, i)) + " <= " + high(i)
          }
        }
          else{   if(overRound){
            lfToString(A.t(::, i)) + " = " + high(i).rounds("Up")}else{
              lfToString(A.t(::, i)) + " = " + high(i)
            }
            }
        }
        eqns.mkString("[ ", " , ", " ]")
      }
    }
  }

}

/**
 * Companion class for the parallelotope domain
 */
object ParallelotopeDomainModQSpire {
  /*private lazy val standard = new ParallelotopeDomainModQSpire(false, true) with CachedTopBottom
  private lazy val favoring = new ParallelotopeDomainModQSpire(true, true) with CachedTopBottom
  private lazy val standard1 = new ParallelotopeDomainModQSpire(false, false) with CachedTopBottom
  private lazy val favoring1 = new ParallelotopeDomainModQSpire(true, false) with CachedTopBottom
  */
  /**
   * Returns an abstract domain for parallelotopes.
   * @param favorAxes determines whether the heuristics built into the domain should try to keep the variability
   * along axes at the minimum. This is generally useful when the domain is one of the component of the sum
   * combinator.
   * if overRound is false the result the values ​​of the results are presented in the form of rational n/d
   */
  //def apply(favorAxes: Boolean = false, overRound: Boolean = true) =if(overRound){ if (favorAxes) favoring else standard} else { if (favorAxes) favoring1 else standard1}  
  //favorAxes = 0 == False, 1 == True, -1 == Removed
  def apply(favorAxes: Integer = 0, overRound: Boolean = true) =if(overRound){ new ParallelotopeDomainModQSpire(favorAxes, true)  with CachedTopBottom} else { new ParallelotopeDomainModQSpire(favorAxes, false) with CachedTopBottom}
}
