/**
 * Copyright 2013 Gianluca Amato
 * 
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
 */

package it.unich.jandom.utils.numberext

import NumberExt.SpecialValues._
import org.gnu.gmp.swig.MPQ
import org.gnu.gmp.swig.MPZ
import breeze.math.Field
import breeze.storage.Zero
import breeze.linalg._
import breeze.linalg.operators._
import org.netlib.util.intW
import it.unich.jandom.utils.breeze.countNonZero
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import it.unich.jandom.utils.numberext.GenericNumberExt.NEGINF

/**
 * 
 * @author Marco Rubino
 * 
 * Note: the operators >,>=,<,<= on NaN always return false. To  be checked  
 */

class ModRationalGmpExt(val value: MPQ, val special: Value) extends NumberExt with Serializable with Ordered[ModRationalGmpExt]{    

	type Extension = ModRationalGmpExt  

			def +(that: ModRationalGmpExt): ModRationalGmpExt = (special,that.special) match {
			case (NORMAL,NORMAL) => {val z= new MPQ(); z.set_add(value,that.value); new ModRationalGmpExt(z,NORMAL);}    
			case (NORMAL,POSINF) =>  ModRationalGmpExt.PositiveInfinity
			case (NORMAL,NEGINF) => ModRationalGmpExt.NegativeInfinity
			case (POSINF,NORMAL) =>  ModRationalGmpExt.PositiveInfinity
			case (NEGINF,NORMAL) => ModRationalGmpExt.NegativeInfinity
			case (POSINF,POSINF) => ModRationalGmpExt.PositiveInfinity
			case (NEGINF,NEGINF) => ModRationalGmpExt.NegativeInfinity
			case (NEGINF,POSINF) => ModRationalGmpExt.NaN
			case (POSINF,NEGINF) => ModRationalGmpExt.NaN
			case (NAN,NAN) => ModRationalGmpExt.NaN
			case (_,NAN) => ModRationalGmpExt.NaN
			case (NAN,_) => ModRationalGmpExt.NaN
			// case (NORMAL,_) =>{ println("$$$ "+special+" "+that.special);that}
			//  case (_,NORMAL) => {println("???? "+special+" "+that.special);this}
			//case (POSINF, _) =>  ModRationalGmpExt.PositiveInfinity
			//case (NEGINF, _) => ModRationalGmpExt.NegativeInfinity 
			//case (_,POSINF) =>  ModRationalGmpExt.PositiveInfinity
			//case (_ , NEGINF) => ModRationalGmpExt.NegativeInfinity 


	}  
	def *(that: ModRationalGmpExt): ModRationalGmpExt = {val zero= new MPQ(); zero.set_d(0); (special,that.special) match {
	case (NORMAL,NORMAL) => {val z= new MPQ(); val zero= new MPQ(); zero.set_d(0);z.set_mul(value, that.value); new ModRationalGmpExt(z,NORMAL)}     

	/* deve dare NaN  [0 * (+/-)Inf]
    case (NORMAL, POSINF) =>{if(value.equal(zero)) ModRationalGmpExt.NaN else { if(value.get_sgn==1) ModRationalGmpExt.PositiveInfinity else  ModRationalGmpExt.NegativeInfinity } }
    case (POSINF, NORMAL) => {if(that.value.equal(zero)) ModRationalGmpExt.NaN else { if(that.value.get_sgn==1) ModRationalGmpExt.PositiveInfinity else  ModRationalGmpExt.NegativeInfinity } }
    case (NORMAL, NEGINF) => {if(value.equal(zero)) ModRationalGmpExt.NaN else { if(value.get_sgn==1) ModRationalGmpExt.NegativeInfinity else  ModRationalGmpExt.PositiveInfinity } }
    case (NEGINF, NORMAL) => {if(that.value.equal(zero)) ModRationalGmpExt.NaN else { if(that.value.get_sgn==1) ModRationalGmpExt.NegativeInfinity else  ModRationalGmpExt.PositiveInfinity } }
	 * 
	 */
	/* solo nel dominio dei parallelotopo  inizio*/
	case (NORMAL, POSINF) =>{if(value.equal(zero)) {val z= new MPQ(); val zero= new MPQ(); zero.set_d(0); new ModRationalGmpExt(z,NORMAL)}else { if(value.get_sgn==1) ModRationalGmpExt.PositiveInfinity else  ModRationalGmpExt.NegativeInfinity } }
	case (POSINF, NORMAL) => {if(that.value.equal(zero)) {val z= new MPQ(); val zero= new MPQ(); zero.set_d(0); new ModRationalGmpExt(z,NORMAL)}else { if(that.value.get_sgn==1) ModRationalGmpExt.PositiveInfinity else  ModRationalGmpExt.NegativeInfinity } }
	case (NORMAL, NEGINF) => {if(value.equal(zero)) {val z= new MPQ(); val zero= new MPQ(); zero.set_d(0); new ModRationalGmpExt(z,NORMAL)}else { if(value.get_sgn==1) ModRationalGmpExt.NegativeInfinity else  ModRationalGmpExt.PositiveInfinity } }
	case (NEGINF, NORMAL) => {if(that.value.equal(zero)) {val z= new MPQ(); val zero= new MPQ(); zero.set_d(0); new ModRationalGmpExt(z,NORMAL)} else { if(that.value.get_sgn==1) ModRationalGmpExt.NegativeInfinity else  ModRationalGmpExt.PositiveInfinity } }
	/* fine */

	case (POSINF, NEGINF) => ModRationalGmpExt.NegativeInfinity
	case (NEGINF, POSINF) => ModRationalGmpExt.NegativeInfinity
	case (POSINF, POSINF) => ModRationalGmpExt.PositiveInfinity
	case (NEGINF, NEGINF) => ModRationalGmpExt.PositiveInfinity    
	case (NAN , NAN) => ModRationalGmpExt.NaN
	case (NAN, _) => ModRationalGmpExt.NaN
	case (_ , NAN) => ModRationalGmpExt.NaN
	/*case (NORMAL,_) => that
    case (_,NORMAL) => this 
    case _ => that*/
	}  
	} 
	def /(that: ModRationalGmpExt): ModRationalGmpExt ={ (special,that.special) match {      
	case (NORMAL,NORMAL) => {  val zero= new MPQ(); zero.set_d(0);
	if(!that.value.equal(zero)){val z= new MPQ(); z.set_div(value, that.value); return new ModRationalGmpExt(z,NORMAL)}else{ 
		if(!value.equal(zero)){                
			if(value.get_sgn == 1)
				return ModRationalGmpExt.PositiveInfinity
						else
							return ModRationalGmpExt.NegativeInfinity

		} 
		else  {  

			ModRationalGmpExt.NaN
		}
	}      
	}

	case (NORMAL, POSINF) =>{val zero= new MPQ(); zero.set_d(0); new ModRationalGmpExt(zero,NORMAL)}
	case (NORMAL, NEGINF) =>{val zero= new MPQ(); zero.set_d(0); new ModRationalGmpExt(zero,NORMAL)}
	// da controllare
	case (POSINF, NORMAL) =>{ if(that.value.get_sgn==1) ModRationalGmpExt.PositiveInfinity else ModRationalGmpExt.NegativeInfinity}
	case (NEGINF, NORMAL) =>{ if(that.value.get_sgn==1) ModRationalGmpExt.NegativeInfinity else ModRationalGmpExt.PositiveInfinity}
	// fine
	case (POSINF, NEGINF) => ModRationalGmpExt.NaN
	case (NEGINF, POSINF) => ModRationalGmpExt.NaN
	case (POSINF, POSINF) => ModRationalGmpExt.NaN
	case (NEGINF, NEGINF) => ModRationalGmpExt.NaN
	case (NAN , NAN) => ModRationalGmpExt.NaN
	case (NAN, _) => ModRationalGmpExt.NaN
	case (_ , NAN) => ModRationalGmpExt.NaN    


	}  
	}
	override def unary_+ = this


			def -(that: ModRationalGmpExt): ModRationalGmpExt = (special,that.special) match {
			case (NORMAL,NORMAL) => {val z= new MPQ(); z.set_sub(value,that.value); new ModRationalGmpExt(z,NORMAL);}    
			case (NORMAL,POSINF) =>  ModRationalGmpExt.NegativeInfinity
			case (NORMAL ,NEGINF) => ModRationalGmpExt.PositiveInfinity
			case (POSINF,NORMAL) =>  ModRationalGmpExt.PositiveInfinity
			case (NEGINF, NORMAL ) => ModRationalGmpExt.NegativeInfinity 
			case (NEGINF, POSINF) =>  ModRationalGmpExt.NegativeInfinity
			case (POSINF,NEGINF) =>  ModRationalGmpExt.PositiveInfinity
			case (POSINF,POSINF) =>  ModRationalGmpExt.NaN 
			case (NEGINF,NEGINF) =>  ModRationalGmpExt.NaN   
			case (NAN,NAN) => ModRationalGmpExt.NaN 
			case (_,NAN) =>  ModRationalGmpExt.NaN 
			case (NAN,_) =>  ModRationalGmpExt.NaN

			//case (NORMAL,_) =>{ println("$$$ "+special+" "+that.special);that}
			//case (_,NORMAL) => {val r = new MPQ();r.set_neg(this.value); new ModRationalGmpExt(r,NORMAL);}
	}  

	def pow(that: ModRationalGmpExt): ModRationalGmpExt = {println("XXXXXXXXXXXXXXXXXXxx");(special,that.special) match {
	case (NORMAL,NORMAL) => {
		var res = new MPQ();
		res.set_d(1)
		for (i <- 0 until that.value.get_d.toInt) {res.set_mul(res, value)}  
		new ModRationalGmpExt(res,NORMAL)}
	case (NORMAL,_) => that
	case (_,NORMAL) => this
	case (POSINF, NEGINF) => ModRationalGmpExt.NaN
	case (NEGINF, POSINF) => ModRationalGmpExt.NaN
	case _ => that
	}
	}

	def isInfinite = special match {
	case POSINF => true
	case NEGINF => true
	case _ =>  false
	} 
	def isPosInfinity = special match {
	case POSINF => true
	case _ =>  false
	}
	def isNegInfinity = special match {
	case NEGINF => true
	case _ =>  false
	}
	def isInfinity =  isPosInfinity || isNegInfinity

			def compare(b: ModRationalGmpExt) = value.cmp(b.value)

			def unary_- = special match {
			case NORMAL => {val z= new MPQ();z.set_neg(value);new ModRationalGmpExt(z, NORMAL)}
			case POSINF => ModRationalGmpExt.NegativeInfinity
			case NEGINF => ModRationalGmpExt.PositiveInfinity
			case NAN => this
	}  

	override def toString = special match {
	case NORMAL => value.toString()
	case POSINF => "+Inf"
	case NEGINF => "-Inf"
	case NAN => "NaN"  
	}  

	override def doubleValue = special match {
	case POSINF => scala.Double.PositiveInfinity
	case NEGINF => scala.Double.NegativeInfinity
	case NAN => scala.Double.NaN
	case NORMAL => value.get_d
	}

	override def floatValue = special match {
	case POSINF => scala.Float.PositiveInfinity
	case NEGINF => scala.Float.NegativeInfinity
	case NAN => scala.Float.NaN
	case NORMAL => value.get_d.toFloat
	}  

	override def longValue = special match {
	case POSINF => Long.MaxValue
	case NEGINF => Long.MinValue
	case NAN => throw new IllegalArgumentException("cannot convert NaN to Long")
	case NORMAL => value.get_d.toLong
	}

	override def intValue = special match {
	case POSINF => Int.MaxValue
	case NEGINF => Int.MinValue
	case NAN => throw new IllegalArgumentException("cannot convert NaN to Long")
	case NORMAL => value.get_d.toInt
	}    

	def max(that: ModRationalGmpExt): ModRationalGmpExt =  (special,that.special) match {
	case (NORMAL,NORMAL) => {value.cmp(that.value)  match {
	case 1 => new ModRationalGmpExt(value,NORMAL)
	case _ => new ModRationalGmpExt(that.value,NORMAL)
	}  
	}
	case (NEGINF, POSINF) => ModRationalGmpExt.PositiveInfinity 
	case (POSINF, NEGINF) => ModRationalGmpExt.PositiveInfinity
	case (POSINF, POSINF) => ModRationalGmpExt.PositiveInfinity
	case (POSINF, NORMAL) => ModRationalGmpExt.PositiveInfinity
	case (NEGINF, NORMAL) => that
	case (NORMAL,NEGINF) =>  this
	case (NORMAL, POSINF) => ModRationalGmpExt.PositiveInfinity
	case (NEGINF, NEGINF) => ModRationalGmpExt.NegativeInfinity
  case (NAN, _) => ModRationalGmpExt.NaN
  case (_, NAN) => ModRationalGmpExt.NaN
	//case (_, POSINF) => ModRationalGmpExt.PositiveInfinity
	//case (_, NEGINF) =>  ModRationalGmpExt.NaN
	//case (POSINF, _) => ModRationalGmpExt.PositiveInfinity
	// case (NEGINF, _) => ModRationalGmpExt.NaN

	} 



	def min(that: ModRationalGmpExt): ModRationalGmpExt = (special,that.special) match {
	case (NORMAL,NORMAL) => { value.cmp(that.value)  match {
	case -1 =>new ModRationalGmpExt(value,NORMAL)
	case _ => new ModRationalGmpExt(that.value,NORMAL)
	}

	}   

	case (NEGINF, POSINF) => ModRationalGmpExt.NegativeInfinity 
	case (POSINF, NEGINF) => ModRationalGmpExt.NegativeInfinity
	case (POSINF, POSINF) => ModRationalGmpExt.PositiveInfinity
	case (NEGINF, NEGINF) => ModRationalGmpExt.NegativeInfinity
	case (NEGINF, NORMAL) => ModRationalGmpExt.NegativeInfinity
	case (NORMAL,NEGINF) => ModRationalGmpExt.NegativeInfinity
	case (NORMAL, POSINF) => this
	case (POSINF,NORMAL ) => that
  case (NAN, _) => ModRationalGmpExt.NaN
  case (_, NAN) => ModRationalGmpExt.NaN

	} 



	override def >(that: ModRationalGmpExt) =(special,that.special) match {
	case (NORMAL,NORMAL) => {  value.cmp(that.value)  match {
	case 1 => true
	case _ =>  false
	}

	} 
	case (POSINF,POSINF) => false
	case (NEGINF, NEGINF) => false
	case (POSINF, _) => true
	case (_, POSINF) => false
	case (NEGINF, _) => false
	case (_,NEGINF) => true
	case (NAN,_) => false
	case (_,NAN) => false

	}



	override def >=(that: ModRationalGmpExt) = (special,that.special) match {
	case (NORMAL,NORMAL) => { value.cmp(that.value)  match {
	case -1 =>  false
	case _ => true
	}
	}

	case (POSINF,POSINF) => true
	case (NEGINF, NEGINF) => true
	case (POSINF, _) => true
	case (_, POSINF) => false
	case (NEGINF, _) => false
	case (_,NEGINF) => true
	case (NAN,_) => false
	case (_,NAN) => false

	}





	override def <(that: ModRationalGmpExt): Boolean = 
    that> this



	override def <=(that: ModRationalGmpExt): Boolean = 	
  (that >= this)



	def !=(that: ModRationalGmpExt) : Boolean = (special,that.special) match {
	case (NORMAL,NORMAL) => {value.cmp(that.value) match {    
	case 0 => false
	case _ => true   
	} 
	}
	case (NAN,NAN) => false 
	case (POSINF, NEGINF) => true
	case (NEGINF, POSINF) => true
	case (POSINF,POSINF) => false
	case (NEGINF, NEGINF) => false   
	case (NORMAL,_) => true
	case (_,NORMAL) => true    


	} 
	def ==(that: ModRationalGmpExt) : Boolean = !(this!=that)
	
	/* override def equals(other: Any): Boolean = {
    other match {
      case other: ModRationalGmpExt => this.value == other.value && this.special == other.special
      case _ => false
    }        
  }*/
  def toDouble: Double = value.get_d

}
object ModRationalGmpExt { outer =>    

val zero = ModRationalGmpExt(0.0)

val one = ModRationalGmpExt(1.0)

val PositiveInfinity = { val ze= new MPQ();ze.set_d(0);new ModRationalGmpExt(ze, POSINF)}

val NegativeInfinity = {val ze= new MPQ();ze.set_d(0); new ModRationalGmpExt(ze, NEGINF)}

val NaN = {val ze= new MPQ();ze.set_d(0);new ModRationalGmpExt(ze, NAN)}

def apply(d: Double): ModRationalGmpExt = {

		d.toString() match {
		case  "Infinity"  =>{ModRationalGmpExt.PositiveInfinity}
		case "-Infinity" =>{ ModRationalGmpExt.NegativeInfinity}
		case  _ =>{ val r= new MPQ();r.set_d(d);  new ModRationalGmpExt(r,NORMAL)}
		}    

}

implicit object scalar extends Field[ModRationalGmpExt] {
	def zero = outer.zero

			def one = outer.one

			def ==(a: ModRationalGmpExt, b:ModRationalGmpExt) = a == b

			def !=(a: ModRationalGmpExt, b:ModRationalGmpExt) = a != b

			def +(a: ModRationalGmpExt, b: ModRationalGmpExt) = a + b

			def -(a: ModRationalGmpExt, b: ModRationalGmpExt) = a - b

			def *(a: ModRationalGmpExt, b: ModRationalGmpExt) = a * b

			def /(a: ModRationalGmpExt, b: ModRationalGmpExt) = a / b

			def %(a: ModRationalGmpExt, b: ModRationalGmpExt) = outer.zero

			def pow(a: ModRationalGmpExt, b: ModRationalGmpExt) = a pow b

			def >(a: ModRationalGmpExt, b: ModRationalGmpExt) = a > b

			def >=(a: ModRationalGmpExt, b: ModRationalGmpExt) = a >= b

			def <(a: ModRationalGmpExt, b: ModRationalGmpExt) = a < b

			def <=(a: ModRationalGmpExt, b: ModRationalGmpExt) = a <= b

			implicit val normImpl: norm.Impl[ModRationalGmpExt, Double] = new norm.Impl[ModRationalGmpExt, Double] {
				def apply(v: ModRationalGmpExt): Double =v.value.get_d
			}

}


trait ModRationalGmpExtNumeric extends Numeric[ModRationalGmpExt] {
	def plus(x: ModRationalGmpExt, y: ModRationalGmpExt): ModRationalGmpExt = x + y
			def minus(x: ModRationalGmpExt, y: ModRationalGmpExt): ModRationalGmpExt = x - y
			def times(x: ModRationalGmpExt, y: ModRationalGmpExt): ModRationalGmpExt = x * y
			def negate(x: ModRationalGmpExt): ModRationalGmpExt = -x
			def fromInt(x: Int): ModRationalGmpExt = ModRationalGmpExt(x.toDouble)
			def toInt(x: ModRationalGmpExt): Int = x.value.get_d().toInt
			def toLong(x: ModRationalGmpExt): Long = x.value.get_d().toLong
			def toFloat(x: ModRationalGmpExt): Float = x.value.get_d().toFloat
			def toDouble(x: ModRationalGmpExt): Double = x.value.get_d()
}

trait ModRationalGmpExtFractional extends ModRationalGmpExtNumeric with Fractional[ModRationalGmpExt] {
	def div(x: ModRationalGmpExt, y: ModRationalGmpExt): ModRationalGmpExt = x / y
}

trait ModRationalGmpExtOrdering extends Ordering[ModRationalGmpExt] {
	override def compare(a : ModRationalGmpExt, b : ModRationalGmpExt) = a.value.cmp(b.value)
}

implicit object ModRationalGmpExtFractional extends ModRationalGmpExtFractional with ModRationalGmpExtOrdering

implicit object MulMM extends OpMulMatrix.Impl2[ModRationalGmpExt,ModRationalGmpExt,ModRationalGmpExt]
		{ def apply(a : ModRationalGmpExt, b : ModRationalGmpExt) = a * b}

implicit object MulDM extends OpDiv.Impl2[Double,ModRationalGmpExt,ModRationalGmpExt]
		{ def apply(a : Double, b : ModRationalGmpExt) = ModRationalGmpExt(a) * b }

implicit object ModRationalGmpExtZero extends Zero[ModRationalGmpExt] {
	val zero = outer.zero
}
implicit def dv_s_Op_ModRationalGmpExt_OpMulMatrix: OpMulMatrix.Impl2[DenseVector[ModRationalGmpExt], ModRationalGmpExt, DenseVector[ModRationalGmpExt]] =
new OpMulMatrix.Impl2[DenseVector[ModRationalGmpExt], ModRationalGmpExt, DenseVector[ModRationalGmpExt]] {
		def apply(a: DenseVector[ModRationalGmpExt], b: ModRationalGmpExt): DenseVector[ModRationalGmpExt] = {
				val ad = a.data
						var aoff = a.offset
						val result = DenseVector.zeros[ModRationalGmpExt](a.length)
						val rd = result.data
						var i = 0
						while (i < a.length) {
							rd(i) = ad(aoff) * b
									aoff += a.stride
									i += 1
						}
				result
		}
		implicitly[BinaryRegistry[Vector[ModRationalGmpExt], ModRationalGmpExt, OpMulMatrix.type, Vector[ModRationalGmpExt]]].register(this)
	}

	implicit object implOpSolveMatrixBy_DRR_DRR_eq_DRR
	extends OpSolveMatrixBy.Impl2[DenseMatrix[ModRationalGmpExt], DenseMatrix[ModRationalGmpExt], DenseMatrix[ModRationalGmpExt]] {

		def LUSolve(X: DenseMatrix[ModRationalGmpExt], A: DenseMatrix[ModRationalGmpExt]) = {

			var perm = (0 until A.rows).toArray        
					for (i <- 0 until A.rows) {              
						val optPivot = (i until A.rows) find { p => A(perm(p), perm(i)) != ModRationalGmpExt.zero}
						val pivotRow = optPivot.getOrElse(throw new MatrixSingularException())              
								val tmp = perm(i)                   
								perm(i) = perm(pivotRow)                 
								perm(pivotRow) = tmp                   
								val pivot = A(perm(i), perm(i))                 
								for (j <- i + 1 until A.rows) {
									val coeff = A(perm(j),perm(i)) / pivot
											A(perm(j), ::) -= A(perm(i), ::) * coeff
											X(perm(j), ::) -= X(perm(i), ::) * coeff
								}

					}

			for (i <- A.rows - 1 to (0, -1)) {   

				X(perm(i), ::) /= A(perm(i), perm(i))          
						for (j <- i - 1 to (0, -1)) {                
							X(perm(j), ::) -= X(perm(i), ::) * A(perm(j), perm(i))              
						}
			}
		}
		override def apply(A: DenseMatrix[ModRationalGmpExt], V: DenseMatrix[ModRationalGmpExt]): DenseMatrix[ModRationalGmpExt] = {
			require(A.rows == V.rows, "Non-conformant matrix sizes")

			if (A.size == 0) {
				DenseMatrix.zeros[ModRationalGmpExt](0, 0)
			} else if (A.rows == A.cols) {
				val X = DenseMatrix.zeros[ModRationalGmpExt](V.rows, V.cols)
						val Y = DenseMatrix.zeros[ModRationalGmpExt](A.rows, A.cols)
						// square: LUSolve

						X := V
						Y := A
						LUSolve(X, Y)

						X
			} else
				???
		}
	}
	implicit object implOpSolveMatrixBy_DMR_DVR_eq_DVR
	extends OpSolveMatrixBy.Impl2[DenseMatrix[ModRationalGmpExt], DenseVector[ModRationalGmpExt], DenseVector[ModRationalGmpExt]] {
		override def apply(a: DenseMatrix[ModRationalGmpExt], b: DenseVector[ModRationalGmpExt]): DenseVector[ModRationalGmpExt] = {
				val rv: DenseMatrix[ModRationalGmpExt] = a \ new DenseMatrix[ModRationalGmpExt](b.size, 1, b.data, b.offset, b.stride, true)
						new DenseVector[ModRationalGmpExt](rv.data)
		}
	}
	implicit def countFromTraverseModRationalGmpExt[T](implicit traverse: CanTraverseValues[T, ModRationalGmpExt]): countNonZero.Impl[T, Int] = {
			new countNonZero.Impl[T, Int] {
				def apply(t: T): Int = {
						var count: Int = 0
								traverse.traverse(t, new ValuesVisitor[ModRationalGmpExt] {
									def visit(a: ModRationalGmpExt) = { if (a != ModRationalGmpExt.zero) count += 1 }
									def zeros(count: Int, zeroValue: ModRationalGmpExt) {}
								})
								count
				}
			}
	}

}