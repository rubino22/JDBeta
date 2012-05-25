/**
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 *
 * (c) 2012 Gianluca Amato
 */
package it.unich.sci.jandom

import scalala.scalar.Scalar
import scalala.collection.sparse.DefaultArrayValue
import scalala.tensor.dense.DenseMatrix
import scalala.operators._

/**
 * This package extends scalala with support for BigDecimal.
 * @author Gianluca Amato <g.amato@unich.it>
 */

package object scalalaext {
  
  implicit object BooleanDefaultArrayValue extends DefaultArrayValue[Boolean] {
    override def value = false;
  }
  
  implicit object ScalarBD extends Scalar[BigDecimal] {
    def zero = BigDecimal(0)
    def one = BigDecimal(1)
    def nan = throw new ArithmeticException("Operation resulted in BigDecimal-valued NaN");
    def ==(a : BigDecimal, b : BigDecimal) = a == b;
    def !=(a : BigDecimal, b : BigDecimal) = a != b;
    def >(a : BigDecimal, b : BigDecimal) = a > b;
    def >=(a : BigDecimal, b : BigDecimal) = a >= b;
    def <(a : BigDecimal, b : BigDecimal) = a < b;
    def <=(a : BigDecimal, b : BigDecimal) = a <= b;
    def +(a : BigDecimal, b : BigDecimal) = a + b;
    def -(a : BigDecimal, b : BigDecimal) = a - b;
    def *(a : BigDecimal, b : BigDecimal) = a * b;
    def /(a : BigDecimal, b : BigDecimal) = a / b;
    def norm(a : BigDecimal) = if (a < 0) -a.toDouble else a.toDouble
    def toDouble(a : BigDecimal) = a.toDouble;
    def isNaN(a : BigDecimal) = false;
    val manifest = implicitly[ClassManifest[BigDecimal]];
    val defaultArrayValue = implicitly[DefaultArrayValue[BigDecimal]];
  }
  
  // BigDecimal <-> Float
  
  implicit object OpAddBDF extends BinaryOp[BigDecimal,Float,OpAdd,BigDecimal]
    { def opType = OpAdd; def apply(a : BigDecimal, b : Float) = a + b; }
  
  implicit object OpSubBDF extends BinaryOp[BigDecimal,Float,OpSub,BigDecimal]
    { def opType = OpSub; def apply(a : BigDecimal, b : Float) = a - b; }
  
  implicit object OpMulBDF extends BinaryOp[BigDecimal,Float,OpMul,BigDecimal]
    { def opType = OpMul; def apply(a : BigDecimal, b : Float) = a * b; }
  
  implicit object OpDivBDF extends BinaryOp[BigDecimal,Float,OpDiv,BigDecimal]
    { def opType = OpDiv; def apply(a : BigDecimal, b : Float) = a / b; }
  
  implicit object OpModBDF extends BinaryOp[BigDecimal,Float,OpMod,BigDecimal]
    { def opType = OpMod; def apply(a : BigDecimal, b : Float) = a % b; }

  implicit object OpLTBDF extends BinaryOp[BigDecimal,Float,OpLT,Boolean]
    { def opType = OpLT; def apply(a : BigDecimal, b : Float) = a < b; }
  
  implicit object OpLTEBDF extends BinaryOp[BigDecimal,Float,OpLTE,Boolean]
    { def opType = OpLTE; def apply(a : BigDecimal, b : Float) = a <= b; }
  
  implicit object OpGTBDF extends BinaryOp[BigDecimal,Float,OpGT,Boolean]
    { def opType = OpGT; def apply(a : BigDecimal, b : Float) = a > b; }
  
  implicit object OpGTEBDF extends BinaryOp[BigDecimal,Float,OpGTE,Boolean]
    { def opType = OpGTE; def apply(a : BigDecimal, b : Float) = a >= b; }
  
  implicit object OpEqBDF extends BinaryOp[BigDecimal,Float,OpEq,Boolean]
    { def opType = OpEq; def apply(a : BigDecimal, b : Float) = a == b; }
  
  implicit object OpNeBDF extends BinaryOp[BigDecimal,Float,OpNe,Boolean]
    { def opType = OpNe; def apply(a : BigDecimal, b : Float) = a != b; }
  
  implicit object OpAndBDF extends BinaryOp[BigDecimal,Float,OpAnd,Boolean]
    { def opType = OpAnd; def apply(a : BigDecimal, b : Float) = a != 0 && b != 0; }
  
  implicit object OpOrBDF extends BinaryOp[BigDecimal,Float,OpOr,Boolean]
    { def opType = OpOr; def apply(a : BigDecimal, b : Float) = a != 0 || b != 0; }

  implicit object OpXorBDF extends BinaryOp[BigDecimal,Float,OpXor,Boolean]
    { def opType = OpXor; def apply(a : BigDecimal, b : Float) = (a != 0) ^ (b != 0); }

  
  // BigDecimal <-> Double
  
  implicit object OpAddBDD extends BinaryOp[BigDecimal,Double,OpAdd,BigDecimal]
    { def opType = OpAdd; def apply(a : BigDecimal, b : Double) = a + b; }
  
  implicit object OpSubBDD extends BinaryOp[BigDecimal,Double,OpSub,BigDecimal]
    { def opType = OpSub; def apply(a : BigDecimal, b : Double) = a - b; }
  
  implicit object OpMulBDD extends BinaryOp[BigDecimal,Double,OpMul,BigDecimal]
    { def opType = OpMul; def apply(a : BigDecimal, b : Double) = a * b; }
  
  implicit object OpDivBDD extends BinaryOp[BigDecimal,Double,OpDiv,BigDecimal]
    { def opType = OpDiv; def apply(a : BigDecimal, b : Double) = a / b; }
  
  implicit object OpModBDD extends BinaryOp[BigDecimal,Double,OpMod,BigDecimal]
    { def opType = OpMod; def apply(a : BigDecimal, b : Double) = a % b; }

  implicit object OpLTBDD extends BinaryOp[BigDecimal,Double,OpLT,Boolean]
    { def opType = OpLT; def apply(a : BigDecimal, b : Double) = a < b; }
  
  implicit object OpLTEBDD extends BinaryOp[BigDecimal,Double,OpLTE,Boolean]
    { def opType = OpLTE; def apply(a : BigDecimal, b : Double) = a <= b; }
  
  implicit object OpGTBDD extends BinaryOp[BigDecimal,Double,OpGT,Boolean]
    { def opType = OpGT; def apply(a : BigDecimal, b : Double) = a > b; }
  
  implicit object OpGTEBDD extends BinaryOp[BigDecimal,Double,OpGTE,Boolean]
    { def opType = OpGTE; def apply(a : BigDecimal, b : Double) = a >= b; }
  
  implicit object OpEqBDD extends BinaryOp[BigDecimal,Double,OpEq,Boolean]
    { def opType = OpEq; def apply(a : BigDecimal, b : Double) = a == b; }
  
  implicit object OpNeBDD extends BinaryOp[BigDecimal,Double,OpNe,Boolean]
    { def opType = OpNe; def apply(a : BigDecimal, b : Double) = a != b; }
  
  implicit object OpAndBDD extends BinaryOp[BigDecimal,Double,OpAnd,Boolean]
    { def opType = OpAnd; def apply(a : BigDecimal, b : Double) = a != 0 && b != 0; }
  
  implicit object OpOrBDD extends BinaryOp[BigDecimal,Double,OpOr,Boolean]
    { def opType = OpOr; def apply(a : BigDecimal, b : Double) = a != 0 || b != 0; }

  implicit object OpXorBDD extends BinaryOp[BigDecimal,Double,OpXor,Boolean]
    { def opType = OpXor; def apply(a : BigDecimal, b : Double) = (a != 0) ^ (b != 0); }
 
  // BigDecimal <-> Int
  
   implicit object OpAddBDI extends BinaryOp[BigDecimal,Int,OpAdd,BigDecimal]
    { def opType = OpAdd; def apply(a : BigDecimal, b : Int) = a + b; }
  
  implicit object OpSubBDI extends BinaryOp[BigDecimal,Int,OpSub,BigDecimal]
    { def opType = OpSub; def apply(a : BigDecimal, b : Int) = a - b; }
  
  implicit object OpMulBDI extends BinaryOp[BigDecimal,Int,OpMul,BigDecimal]
    { def opType = OpMul; def apply(a : BigDecimal, b : Int) = a * b; }
  
  implicit object OpDivBDI extends BinaryOp[BigDecimal,Int,OpDiv,BigDecimal]
    { def opType = OpDiv; def apply(a : BigDecimal, b : Int) = a / b; }
  
  implicit object OpModBDI extends BinaryOp[BigDecimal,Int,OpMod,BigDecimal]
    { def opType = OpMod; def apply(a : BigDecimal, b : Int) = a % b; }
  
  implicit object OpPowBDI extends BinaryOp[BigDecimal,Int,OpPow,BigDecimal]
    { def opType = OpPow; def apply(a : BigDecimal, b : Int) = a.pow(b); }
  
  implicit object OpLTBDI extends BinaryOp[BigDecimal,Int,OpLT,Boolean]
    { def opType = OpLT; def apply(a : BigDecimal, b : Int) = a < b; }
  
  implicit object OpLTEBDI extends BinaryOp[BigDecimal,Int,OpLTE,Boolean]
    { def opType = OpLTE; def apply(a : BigDecimal, b : Int) = a <= b; }
  
  implicit object OpGTBDI extends BinaryOp[BigDecimal,Int,OpGT,Boolean]
    { def opType = OpGT; def apply(a : BigDecimal, b : Int) = a > b; }
  
  implicit object OpGTEBDI extends BinaryOp[BigDecimal,Int,OpGTE,Boolean]
    { def opType = OpGTE; def apply(a : BigDecimal, b : Int) = a >= b; }
  
  implicit object OpEqBDI extends BinaryOp[BigDecimal,Int,OpEq,Boolean]
    { def opType = OpEq; def apply(a : BigDecimal, b : Int) = a == b; }
  
  implicit object OpNeBDI extends BinaryOp[BigDecimal,Int,OpNe,Boolean]
    { def opType = OpNe; def apply(a : BigDecimal, b : Int) = a != b; }
  
  implicit object OpAndBDI extends BinaryOp[BigDecimal,Int,OpAnd,Boolean]
    { def opType = OpAnd; def apply(a : BigDecimal, b : Int) = a != 0 && b != 0; }
  
  implicit object OpOrBDI extends BinaryOp[BigDecimal,Int,OpOr,Boolean]
    { def opType = OpOr; def apply(a : BigDecimal, b : Int) = a != 0 || b != 0; }

  implicit object OpXorBDI extends BinaryOp[BigDecimal,Int,OpXor,Boolean]
    { def opType = OpXor; def apply(a : BigDecimal, b : Int) = (a != 0) ^ (b != 0); }

  
  // BigDecimal <-> Long
  
  implicit object OpAddBDL extends BinaryOp[BigDecimal,Long,OpAdd,BigDecimal]
    { def opType = OpAdd; def apply(a : BigDecimal, b : Long) = a + b; }
  
  implicit object OpSubBDL extends BinaryOp[BigDecimal,Long,OpSub,BigDecimal]
    { def opType = OpSub; def apply(a : BigDecimal, b : Long) = a - b; }
  
  implicit object OpMulBDL extends BinaryOp[BigDecimal,Long,OpMul,BigDecimal]
    { def opType = OpMul; def apply(a : BigDecimal, b : Long) = a * b; }
  
  implicit object OpDivBDL extends BinaryOp[BigDecimal,Long,OpDiv,BigDecimal]
    { def opType = OpDiv; def apply(a : BigDecimal, b : Long) = a / b; }
  
  implicit object OpModBDL extends BinaryOp[BigDecimal,Long,OpMod,BigDecimal]
    { def opType = OpMod; def apply(a : BigDecimal, b : Long) = a % b; }
  
  implicit object OpLTBDL extends BinaryOp[BigDecimal,Long,OpLT,Boolean]
    { def opType = OpLT; def apply(a : BigDecimal, b : Long) = a < b; }
  
  implicit object OpLTEBDL extends BinaryOp[BigDecimal,Long,OpLTE,Boolean]
    { def opType = OpLTE; def apply(a : BigDecimal, b : Long) = a <= b; }
  
  implicit object OpGTBDL extends BinaryOp[BigDecimal,Long,OpGT,Boolean]
    { def opType = OpGT; def apply(a : BigDecimal, b : Long) = a > b; }
  
  implicit object OpGTEBDL extends BinaryOp[BigDecimal,Long,OpGTE,Boolean]
    { def opType = OpGTE; def apply(a : BigDecimal, b : Long) = a >= b; }
  
  implicit object OpEqBDL extends BinaryOp[BigDecimal,Long,OpEq,Boolean]
    { def opType = OpEq; def apply(a : BigDecimal, b : Long) = a == b; }
  
  implicit object OpNeBDL extends BinaryOp[BigDecimal,Long,OpNe,Boolean]
    { def opType = OpNe; def apply(a : BigDecimal, b : Long) = a != b; }
  
  implicit object OpAndBDL extends BinaryOp[BigDecimal,Long,OpAnd,Boolean]
    { def opType = OpAnd; def apply(a : BigDecimal, b : Long) = a != 0 && b != 0; }
  
  implicit object OpOrBDL extends BinaryOp[BigDecimal,Long,OpOr,Boolean]
    { def opType = OpOr; def apply(a : BigDecimal, b : Long) = a != 0 || b != 0; }

  implicit object OpXorBDL extends BinaryOp[BigDecimal,Long,OpXor,Boolean]
    { def opType = OpXor; def apply(a : BigDecimal, b : Long) = (a != 0) ^ (b != 0); }
  
   // BigDecimal <-> Int
  
   implicit object OpAddBDBD extends BinaryOp[BigDecimal,BigDecimal,OpAdd,BigDecimal]
    { def opType = OpAdd; def apply(a : BigDecimal, b : BigDecimal) = a + b; }
  
  implicit object OpSubBDBD extends BinaryOp[BigDecimal,BigDecimal,OpSub,BigDecimal]
    { def opType = OpSub; def apply(a : BigDecimal, b : BigDecimal) = a - b; }
  
  implicit object OpMulBDBD extends BinaryOp[BigDecimal,BigDecimal,OpMul,BigDecimal]
    { def opType = OpMul; def apply(a : BigDecimal, b : BigDecimal) = a * b; }
  
  implicit object OpDivBDBD extends BinaryOp[BigDecimal,BigDecimal,OpDiv,BigDecimal]
    { def opType = OpDiv; def apply(a : BigDecimal, b : BigDecimal) = a / b; }
  
  implicit object OpModBDBD extends BinaryOp[BigDecimal,BigDecimal,OpMod,BigDecimal]
    { def opType = OpMod; def apply(a : BigDecimal, b : BigDecimal) = a % b; }
  
  implicit object OpLTBDBD extends BinaryOp[BigDecimal,BigDecimal,OpLT,Boolean]
    { def opType = OpLT; def apply(a : BigDecimal, b : BigDecimal) = a < b; }
  
  implicit object OpLTEBDBD extends BinaryOp[BigDecimal,BigDecimal,OpLTE,Boolean]
    { def opType = OpLTE; def apply(a : BigDecimal, b : BigDecimal) = a <= b; }
  
  implicit object OpGTBDBD extends BinaryOp[BigDecimal,BigDecimal,OpGT,Boolean]
    { def opType = OpGT; def apply(a : BigDecimal, b : BigDecimal) = a > b; }
  
  implicit object OpGTEBDBD extends BinaryOp[BigDecimal,BigDecimal,OpGTE,Boolean]
    { def opType = OpGTE; def apply(a : BigDecimal, b : BigDecimal) = a >= b; }
  
  implicit object OpEqBDBD extends BinaryOp[BigDecimal,BigDecimal,OpEq,Boolean]
    { def opType = OpEq; def apply(a : BigDecimal, b : BigDecimal) = a == b; }
  
  implicit object OpNeBDBD extends BinaryOp[BigDecimal,BigDecimal,OpNe,Boolean]
    { def opType = OpNe; def apply(a : BigDecimal, b : BigDecimal) = a != b; }
  
  implicit object OpAndBDBD extends BinaryOp[BigDecimal,BigDecimal,OpAnd,Boolean]
    { def opType = OpAnd; def apply(a : BigDecimal, b : BigDecimal) = a != 0 && b != 0; }
  
  implicit object OpOrBDBD extends BinaryOp[BigDecimal,BigDecimal,OpOr,Boolean]
    { def opType = OpOr; def apply(a : BigDecimal, b : BigDecimal) = a != 0 || b != 0; }

  implicit object OpXorBDBD extends BinaryOp[BigDecimal,BigDecimal,OpXor,Boolean]
    { def opType = OpXor; def apply(a : BigDecimal, b : BigDecimal) = (a != 0) ^ (b != 0); }
  
  //implicit object DenseMatrixCanSliceColBD extends DenseMatrix.DenseMatrixCanSliceCol[BigDecimal];

}