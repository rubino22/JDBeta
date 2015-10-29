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


/**
 * This is the abstract domain of parallelotopes as appears in the NSAD 2012 paper. It is written
 * using the Breeze Math library with the extension numbers using rational numbers library GMP
 *
 * @author Marco Rubino <marco.rubino@unich.it>
 */
package it.unich.jandom.utils.numberext

import NumberExt.SpecialValues._
import org.gnu.gmp.swig.MPQ
import breeze.math.Field
import breeze.storage.Zero
import breeze.linalg._
import breeze.linalg.operators._
import org.netlib.util.intW
import it.unich.jandom.utils.breeze.countNonZero
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import org.gnu.gmp.swig.MPZ
import spire.math.Rational


/**
 * 
 * @author Marco Rubino
 * 
 * Note: the operators >,>=,<,<= on NaN always return false. To  be checked  
 */

class ModRationalSpireExt(val value: Rational, val special: Value) extends NumberExt with Serializable with Ordered[ModRationalSpireExt]{    

	type Extension = ModRationalSpireExt  

			def +(that: ModRationalSpireExt): ModRationalSpireExt = (special,that.special) match {
			case (NORMAL,NORMAL) => { var z1= Rational(0); z1=value + that.value;  new ModRationalSpireExt(z1,NORMAL);}    
			case (NORMAL,POSINF) =>  ModRationalSpireExt.PositiveInfinity
			case (NORMAL,NEGINF) => ModRationalSpireExt.NegativeInfinity
			case (POSINF,NORMAL) =>  ModRationalSpireExt.PositiveInfinity
			case (NEGINF,NORMAL) => ModRationalSpireExt.NegativeInfinity
			case (POSINF,POSINF) => ModRationalSpireExt.PositiveInfinity
			case (NEGINF,NEGINF) => ModRationalSpireExt.NegativeInfinity
			case (NEGINF,POSINF) => ModRationalSpireExt.NaN
			case (POSINF,NEGINF) => ModRationalSpireExt.NaN			
			case (_,NAN) => ModRationalSpireExt.NaN
			case (NAN,_) => ModRationalSpireExt.NaN


	}  
	def *(that: ModRationalSpireExt): ModRationalSpireExt = {val zero= Rational(0); (special,that.special) match {
	case (NORMAL,NORMAL) => {var z= Rational(0);  z= value * that.value; new ModRationalSpireExt(z,NORMAL)}     

	/* deve dare NaN  [0 * (+/-)Inf]
    case (NORMAL, POSINF) =>{if(value.equal(zero)) ModRationalSpireExt.NaN else { if(value.get_sgn==1) ModRationalSpireExt.PositiveInfinity else  ModRationalSpireExt.NegativeInfinity } }
    case (POSINF, NORMAL) => {if(that.value.equal(zero)) ModRationalSpireExt.NaN else { if(that.value.get_sgn==1) ModRationalSpireExt.PositiveInfinity else  ModRationalSpireExt.NegativeInfinity } }
    case (NORMAL, NEGINF) => {if(value.equal(zero)) ModRationalSpireExt.NaN else { if(value.get_sgn==1) ModRationalSpireExt.NegativeInfinity else  ModRationalSpireExt.PositiveInfinity } }
    case (NEGINF, NORMAL) => {if(that.value.equal(zero)) ModRationalSpireExt.NaN else { if(that.value.get_sgn==1) ModRationalSpireExt.NegativeInfinity else  ModRationalSpireExt.PositiveInfinity } }
	 * 
	 */
	/* solo nel dominio dei parallelotopo  inizio*/
	case (NORMAL, POSINF) =>{if(value == (zero)) {var z= Rational(0); val zero= Rational(0); new ModRationalSpireExt(z,NORMAL)}else { if(value.signum == 1) ModRationalSpireExt.PositiveInfinity else  ModRationalSpireExt.NegativeInfinity } }
	case (POSINF, NORMAL) => {if(that.value == (zero)) {var z= Rational(0); val zero= Rational(0); new ModRationalSpireExt(z,NORMAL)}else { if(that.value.signum == 1) ModRationalSpireExt.PositiveInfinity else  ModRationalSpireExt.NegativeInfinity } }
	case (NORMAL, NEGINF) => {if(value  == (zero)) {var z= Rational(0); val zero= Rational(0); new ModRationalSpireExt(z,NORMAL)}else { if(value.signum == 1) ModRationalSpireExt.NegativeInfinity else  ModRationalSpireExt.PositiveInfinity } }
	case (NEGINF, NORMAL) => {if(that.value == (zero)) {var z= Rational(0); val zero= Rational(0); new ModRationalSpireExt(z,NORMAL)} else { if(that.value.signum == 1) ModRationalSpireExt.NegativeInfinity else  ModRationalSpireExt.PositiveInfinity } }
	/* fine */

	case (POSINF, NEGINF) => ModRationalSpireExt.NegativeInfinity
	case (NEGINF, POSINF) => ModRationalSpireExt.NegativeInfinity
	case (POSINF, POSINF) => ModRationalSpireExt.PositiveInfinity
	case (NEGINF, NEGINF) => ModRationalSpireExt.PositiveInfinity
	case (NAN, _) => ModRationalSpireExt.NaN
	case (_ , NAN) => ModRationalSpireExt.NaN
	
	}  
	} 
	def /(that: ModRationalSpireExt): ModRationalSpireExt ={(special,that.special) match {      
	case (NORMAL,NORMAL) => {  val zero= Rational(0);
	if(that.value != zero ){var z= Rational(0); z= value / that.value; return new ModRationalSpireExt(z,NORMAL)}else{ 
		if(value != zero){                
			if(value.signum == 1)
				return ModRationalSpireExt.PositiveInfinity
						else
							return ModRationalSpireExt.NegativeInfinity

		} 
		else  {  

			ModRationalSpireExt.NaN
		}
	}      
	}

	case (NORMAL, POSINF) =>{val zero= Rational(0); new ModRationalSpireExt(zero,NORMAL)}
	case (NORMAL, NEGINF) =>{val zero= Rational(0); new ModRationalSpireExt(zero,NORMAL)}
	case (POSINF, NORMAL) =>{ if(that.value.signum==1) ModRationalSpireExt.PositiveInfinity else ModRationalSpireExt.NegativeInfinity}
	case (NEGINF, NORMAL) =>{ if(that.value.signum==1) ModRationalSpireExt.NegativeInfinity else ModRationalSpireExt.PositiveInfinity}
	case (POSINF, NEGINF) => ModRationalSpireExt.NaN
	case (NEGINF, POSINF) => ModRationalSpireExt.NaN
	case (POSINF, POSINF) => ModRationalSpireExt.NaN
	case (NEGINF, NEGINF) => ModRationalSpireExt.NaN	
	case (NAN, _) => ModRationalSpireExt.NaN
	case (_ , NAN) => ModRationalSpireExt.NaN    


	}  
	}
	override def unary_+ = this


			def -(that: ModRationalSpireExt): ModRationalSpireExt = (special,that.special) match {
			case (NORMAL,NORMAL) => {var z= Rational(0); z=value - that.value; new ModRationalSpireExt(z,NORMAL);}    
			case (NORMAL,POSINF) =>  ModRationalSpireExt.NegativeInfinity
			case (NORMAL ,NEGINF) => ModRationalSpireExt.PositiveInfinity
			case (POSINF,NORMAL) =>  ModRationalSpireExt.PositiveInfinity
			case (NEGINF, NORMAL ) => ModRationalSpireExt.NegativeInfinity 
			case (NEGINF, POSINF) =>  ModRationalSpireExt.NegativeInfinity
			case (POSINF,NEGINF) =>  ModRationalSpireExt.PositiveInfinity
			case (POSINF,POSINF) =>  ModRationalSpireExt.NaN 
			case (NEGINF,NEGINF) =>  ModRationalSpireExt.NaN
			case (_,NAN) =>  ModRationalSpireExt.NaN 
			case (NAN,_) =>  ModRationalSpireExt.NaN

		
	}  

  def pow2(that: ModRationalSpireExt): ModRationalSpireExt = {(special,that.special) match {
  case (NORMAL,NORMAL) => {
    
      var res = Rational(1);    
    //res.set_d(1)
    for (i <- 0 until that.value.doubleValue().toInt) {res= res.*(value)}  
    new ModRationalSpireExt(res,NORMAL)}
  case (NORMAL,_) => that
  case (_,NORMAL) => this
  case (POSINF, NEGINF) => ModRationalSpireExt.NaN
  case (NEGINF, POSINF) => ModRationalSpireExt.NaN
  case (NAN, _) => ModRationalSpireExt.NaN
  case (_,NAN) => ModRationalSpireExt.NaN
  
  }
  }
  
  
	def pow(that: ModRationalSpireExt): ModRationalSpireExt = {(special,that.special) match {
	case (NORMAL,NORMAL) => {
		var res = Rational(0);		
		 {res= value.pow(that.value.doubleValue().toInt)}  
		new ModRationalSpireExt(res,NORMAL)}
	case (NORMAL,_) => that
	case (_,NORMAL) => this
	case (POSINF, NEGINF) => ModRationalSpireExt.NaN
	case (NEGINF, POSINF) => ModRationalSpireExt.NaN
  case (NAN, _) => ModRationalSpireExt.NaN
  case (_,NAN) => ModRationalSpireExt.NaN
	
	}
	}

  def abs(): ModRationalSpireExt = {(special) match {
  case (NORMAL) => {
    var res= Rational(0);
     res=value.abs      
    new ModRationalSpireExt(res,NORMAL)}
  
  case (POSINF) => ModRationalSpireExt.NaN
  case (NEGINF) => ModRationalSpireExt.NaN
  case (NAN) => ModRationalSpireExt.NaN
 
  
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

			def compare(that: ModRationalSpireExt) =(special,that.special) match {
          case (NORMAL,NORMAL) => value.compare(that.value)
        }
 
        
        
        

			def unary_- = special match {
			case NORMAL => {var z= Rational(0);z = value.unary_-(); new ModRationalSpireExt(z, NORMAL)}
			case POSINF => ModRationalSpireExt.NegativeInfinity
			case NEGINF => ModRationalSpireExt.PositiveInfinity
	
      case NAN => ModRationalSpireExt.NaN
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
	case NORMAL => value.doubleValue()
	}

	override def floatValue = special match {
	case POSINF => scala.Float.PositiveInfinity
	case NEGINF => scala.Float.NegativeInfinity
	case NAN => scala.Float.NaN
	case NORMAL => value.doubleValue().toFloat
	}  

	override def longValue = special match {
	case POSINF => Long.MaxValue
	case NEGINF => Long.MinValue
	case NAN => throw new IllegalArgumentException("cannot convert NaN to Long")
	case NORMAL => value.doubleValue().toLong
	}

	override def intValue = special match {
	case POSINF => Int.MaxValue
	case NEGINF => Int.MinValue
	case NAN => throw new IllegalArgumentException("cannot convert NaN to Long")
	case NORMAL => value.doubleValue().toInt
	}    

	def max(that: ModRationalSpireExt): ModRationalSpireExt =  (special,that.special) match {
	case (NORMAL,NORMAL) => {var z = Rational(0); z= value.max(that.value); new ModRationalSpireExt(z,NORMAL)
    
	}
	case (NEGINF, POSINF) => ModRationalSpireExt.PositiveInfinity 
	case (POSINF, NEGINF) => ModRationalSpireExt.PositiveInfinity
	case (POSINF, POSINF) => ModRationalSpireExt.PositiveInfinity
	case (POSINF, NORMAL) => ModRationalSpireExt.PositiveInfinity
	case (NEGINF, NORMAL) => that
	case (NORMAL,NEGINF) =>  this
	case (NORMAL, POSINF) => ModRationalSpireExt.PositiveInfinity
	case (NEGINF, NEGINF) => ModRationalSpireExt.NegativeInfinity
  case (NAN, _) => ModRationalSpireExt.NaN
  case (_, NAN) => ModRationalSpireExt.NaN
	

	} 



	def min(that: ModRationalSpireExt): ModRationalSpireExt = (special,that.special) match {
	case (NORMAL,NORMAL) => {var z = Rational(0); z= value.min(that.value); new ModRationalSpireExt(z,NORMAL); 
	}   

	case (NEGINF, POSINF) => ModRationalSpireExt.NegativeInfinity 
	case (POSINF, NEGINF) => ModRationalSpireExt.NegativeInfinity
	case (POSINF, POSINF) => ModRationalSpireExt.PositiveInfinity
	case (NEGINF, NEGINF) => ModRationalSpireExt.NegativeInfinity
	case (NEGINF, NORMAL) => ModRationalSpireExt.NegativeInfinity
	case (NORMAL,NEGINF) => ModRationalSpireExt.NegativeInfinity
	case (NORMAL, POSINF) => this
	case (POSINF,NORMAL ) => that
  case (NAN, _) => ModRationalSpireExt.NaN
  case (_, NAN) => ModRationalSpireExt.NaN

	} 



	override def >(that: ModRationalSpireExt) =(special,that.special) match {
	case (NORMAL,NORMAL) => {  value > (that.value)  

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



	override def >=(that: ModRationalSpireExt) = (special,that.special) match {
	case (NORMAL,NORMAL) => { value >= (that.value)  
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





	override def <(that: ModRationalSpireExt): Boolean = (special,that.special) match {
    case (NAN,_) => false
    case (_,NAN) => false
    case (_,_) => that> this
  }


	override def <=(that: ModRationalSpireExt): Boolean =(special,that.special) match { 	
     case (NAN,_) => false
     case (_,NAN) => false
     case (_,_) =>(that >= this)
  }


	def !=(that: ModRationalSpireExt) : Boolean = (special,that.special) match {
	case (NORMAL,NORMAL) => {value != (that.value)  
	  }
	case (NAN,NAN) => false 
	case (POSINF, NEGINF) => true
	case (NEGINF, POSINF) => true
	case (POSINF,POSINF) => false
	case (NEGINF, NEGINF) => false   
	case (NORMAL,_) => true
	case (_,NORMAL) => true    
	} 
  

  override def equals(that: Any) : Boolean = {that match {
    case that: ModRationalSpireExt => 
          {(special,that.special) match {
            case (NAN,_) => false
            case (_,NAN) => false  
            case (_,_) => !(this!=that)
            }
          }
  case _=> false
  }
  }

  
   /* def rounds(roudType: String): String = special match {
     case NORMAL =>{
       val uno = new MPZ(1)  
       val den = new MPZ()
       val num = new MPZ()
       val k= 2
       val base= new MPZ(10)     
       val ris= new MPZ() 
       den.set_d(value.denominator.doubleValue())
       num.set_d(value.numerator.doubleValue())
      
       
       if(uno.cmpabs(den)!=0){
         base.set_pow_ui(base, k)      
         base.set_mul(num, base)
       
         if(roudType.equals("Up")){
           if(num.get_sgn() == -1)ris.set_fdiv_q( base, den) else ris.set_cdiv_q(base, den)
         }else{
           if(num.get_sgn() == -1) ris.set_cdiv_q(base, den) else ris.set_fdiv_q( base, den)           
         }
         
         val lx= ris.get_d.toString()
         val temp= new MPZ();
         temp.set_abs(ris)
         var length2 = ( Math.log10(temp.get_d) + 1).toInt
         var stringa=""
         if(ris.get_sgn() == -1){
           length2=length2+1
         }
         for (i <- 0 until length2) { 
           if(i ==length2-k){
             if(stringa.length()==0){
               stringa=stringa.concat("0")
             }
             stringa=stringa.concat(".");
           }
           stringa=stringa.concat(""+lx.charAt(i))

         }
         return stringa
       }else{// if den equals 1
         value.toString()
       }
     }
     case  POSINF  => ModRationalGmpExt.PositiveInfinity.toString()
     case NEGINF => ModRationalGmpExt.NegativeInfinity.toString()
     case NAN => ModRationalGmpExt.NaN.toString()
   } 
   
    */
    def rounds(roudType: String): String = special match {
     case NORMAL =>{      
       val k= 2     
       val denominatore=value.denominator.doubleValue()
       val numeratore=value.numerator.doubleValue()
       val unoSpire= Rational(1)
       var baseSpire= Rational(10)
       var divisione= Rational(0)
       
      if(unoSpire.compare((denominatore.abs))!=0){
         baseSpire=baseSpire.pow(k)        
         baseSpire=baseSpire.*(numeratore)        
         divisione= baseSpire /(denominatore)
         
          if(roudType.equals("Up")){
           if(value.signum == -1)divisione=divisione.floor else divisione=divisione.ceil
         }else{
           if(value.signum == -1) divisione=divisione.ceil else divisione=divisione.floor           
         }
       
       val lx= divisione.doubleValue().toString()
         val temp= divisione.abs 
         var length2 = ( Math.log10(temp.doubleValue()) + 1).toInt        
         var stringa=""
         if(divisione.signum == -1){
           length2=length2+1
         }
         for (i <- 0 until length2) { 
           if(i ==length2-k){
             if(stringa.length()==0){
               stringa=stringa.concat("0")
             }
             stringa=stringa.concat(".");
           }
           stringa=stringa.concat(""+lx.charAt(i))

         }
         return stringa

       
       
       /*if(uno.cmpabs(den)!=0){
         base.set_pow_ui(base, k)      
         base.set_mul(num, base)
       
         if(roudType.equals("Up")){
           if(num.get_sgn() == -1)ris.set_fdiv_q( base, den) else ris.set_cdiv_q(base, den)
         }else{
           if(num.get_sgn() == -1) ris.set_cdiv_q(base, den) else ris.set_fdiv_q( base, den)           
         }
         
         val lx= ris.get_d.toString()
         val temp= new MPZ();
         temp.set_abs(ris)
         var length2 = ( Math.log10(temp.get_d) + 1).toInt
         var stringa=""
         if(ris.get_sgn() == -1){
           length2=length2+1
         }
         for (i <- 0 until length2) { 
           if(i ==length2-k){
             if(stringa.length()==0){
               stringa=stringa.concat("0")
             }
             stringa=stringa.concat(".");
           }
           stringa=stringa.concat(""+lx.charAt(i))

         }
         return stringa
       }else{// if den equals 1
         value.toString()
       }*/
     } else{
       value.toString()
         }
     }
     case  POSINF  => ModRationalGmpExt.PositiveInfinity.toString()
     case NEGINF => ModRationalGmpExt.NegativeInfinity.toString()
     case NAN => ModRationalGmpExt.NaN.toString()
   } 
   
    
  def toDouble: Double = special match {
  case NORMAL => value.doubleValue()  
  case NEGINF => scala.Double.NegativeInfinity
  case POSINF => scala.Double.PositiveInfinity
  case NAN  => scala.Double.NaN
  
  }

}
object ModRationalSpireExt { outer =>    

val zero = ModRationalSpireExt(0)

val one = ModRationalSpireExt(1)

val PositiveInfinity = { val ze= Rational(0);new ModRationalSpireExt(ze, POSINF)}

val NegativeInfinity = {val ze= Rational(0); new ModRationalSpireExt(ze, NEGINF)}

val NaN = {val ze= Rational(0);new ModRationalSpireExt(ze, NAN)}

def apply(d: Double): ModRationalSpireExt = {
     
		d.toString() match {
		case  "Infinity"  =>{ModRationalSpireExt.PositiveInfinity}
		case "-Infinity" =>{ ModRationalSpireExt.NegativeInfinity}
    case "NAN" =>{ ModRationalSpireExt.NaN}
		case  _ =>{if(d==0){val r= Rational(0);new ModRationalSpireExt(r,NORMAL)}else{val r= Rational(d);  new ModRationalSpireExt(r,NORMAL)}}
		}    

}

implicit object scalar extends Field[ModRationalSpireExt] {
	def zero = outer.zero

			def one = outer.one

			def ==(a: ModRationalSpireExt, b:ModRationalSpireExt) = a == b

			def !=(a: ModRationalSpireExt, b:ModRationalSpireExt) = a != b

			def +(a: ModRationalSpireExt, b: ModRationalSpireExt) = a + b

			def -(a: ModRationalSpireExt, b: ModRationalSpireExt) = a - b

			def *(a: ModRationalSpireExt, b: ModRationalSpireExt) = a * b

			def /(a: ModRationalSpireExt, b: ModRationalSpireExt) = a / b

			def %(a: ModRationalSpireExt, b: ModRationalSpireExt) = outer.zero

			def pow(a: ModRationalSpireExt, b: ModRationalSpireExt) = a pow b

			def >(a: ModRationalSpireExt, b: ModRationalSpireExt) = a > b

			def >=(a: ModRationalSpireExt, b: ModRationalSpireExt) = a >= b

			def <(a: ModRationalSpireExt, b: ModRationalSpireExt) = a < b

			def <=(a: ModRationalSpireExt, b: ModRationalSpireExt) = a <= b
      
      
      def abs(a: ModRationalSpireExt) = a.abs()

			implicit val normImpl: norm.Impl[ModRationalSpireExt, Double] = new norm.Impl[ModRationalSpireExt, Double] {
				def apply(v: ModRationalSpireExt): Double =v.value.doubleValue()
			}

}


trait ModRationalSpireExtNumeric extends Numeric[ModRationalSpireExt] {
	def plus(x: ModRationalSpireExt, y: ModRationalSpireExt): ModRationalSpireExt = x + y
			def minus(x: ModRationalSpireExt, y: ModRationalSpireExt): ModRationalSpireExt = x - y
			def times(x: ModRationalSpireExt, y: ModRationalSpireExt): ModRationalSpireExt = x * y
			def negate(x: ModRationalSpireExt): ModRationalSpireExt = -x
			def fromInt(x: Int): ModRationalSpireExt = ModRationalSpireExt(x.toDouble)
			def toInt(x: ModRationalSpireExt): Int = x.value.doubleValue().toInt
			def toLong(x: ModRationalSpireExt): Long = x.doubleValue().toLong
			def toFloat(x: ModRationalSpireExt): Float = x.doubleValue().toFloat
			def toDouble(x: ModRationalSpireExt): Double = x.doubleValue()
      
}

trait ModRationalSpireExtFractional extends ModRationalSpireExtNumeric with Fractional[ModRationalSpireExt] {
	def div(x: ModRationalSpireExt, y: ModRationalSpireExt): ModRationalSpireExt = x / y
}

trait ModRationalSpireExtOrdering extends Ordering[ModRationalSpireExt] {
	override def compare(a : ModRationalSpireExt, b : ModRationalSpireExt) = a.value.compare(b.value)
}

implicit object ModRationalSpireExtFractional extends ModRationalSpireExtFractional with ModRationalSpireExtOrdering

implicit object MulMM extends OpMulMatrix.Impl2[ModRationalSpireExt,ModRationalSpireExt,ModRationalSpireExt]
		{ def apply(a : ModRationalSpireExt, b : ModRationalSpireExt) = a * b}

implicit object MulDM extends OpDiv.Impl2[Double,ModRationalSpireExt,ModRationalSpireExt]
		{ def apply(a : Double, b : ModRationalSpireExt) = ModRationalSpireExt(a) * b }

implicit object ModRationalSpireExtZero extends Zero[ModRationalSpireExt] {
	val zero = outer.zero
}
implicit def dv_s_Op_ModRationalSpireExt_OpMulMatrix: OpMulMatrix.Impl2[DenseVector[ModRationalSpireExt], ModRationalSpireExt, DenseVector[ModRationalSpireExt]] =
new OpMulMatrix.Impl2[DenseVector[ModRationalSpireExt], ModRationalSpireExt, DenseVector[ModRationalSpireExt]] {
		def apply(a: DenseVector[ModRationalSpireExt], b: ModRationalSpireExt): DenseVector[ModRationalSpireExt] = {
				val ad = a.data
						var aoff = a.offset
						val result = DenseVector.zeros[ModRationalSpireExt](a.length)
						val rd = result.data
						var i = 0
						while (i < a.length) {
							rd(i) = ad(aoff) * b
									aoff += a.stride
									i += 1
						}
				result
		}
		implicitly[BinaryRegistry[Vector[ModRationalSpireExt], ModRationalSpireExt, OpMulMatrix.type, Vector[ModRationalSpireExt]]].register(this)
	}

	implicit object implOpSolveMatrixBy_DRR_DRR_eq_DRR
	extends OpSolveMatrixBy.Impl2[DenseMatrix[ModRationalSpireExt], DenseMatrix[ModRationalSpireExt], DenseMatrix[ModRationalSpireExt]] {

  
   /* 
		def LUSolve(X: DenseMatrix[ModRationalSpireExt], A: DenseMatrix[ModRationalSpireExt]) = {    

       
			var perm = (0 until A.rows).toArray 
					for (i <- 0 until A.rows-1) {  
						//val optPivot = (i until A.rows) find { p =>  A(perm(p),i) != ModRationalSpireExt.zero}
						val optPivotuno = (i until A.rows) find { p =>  A(perm(p),i) == ModRationalSpireExt.one }
						val pivotRow = optPivotuno.getOrElse(((i until A.rows) find { p =>  A(perm(p),i) != ModRationalSpireExt.zero}).getOrElse(throw new MatrixSingularException()))
						  val tmp = perm(i)
                      perm(i) = perm(pivotRow)
                      perm(pivotRow) = tmp
		
            if(pivotRow == 1){
																			//val pivot = A(perm(i), i)                 
											for (j <- i + 1 until A.rows) {
												val coeff = A(perm(j),i)                  
														A(perm(j), ::) -= A(perm(i), ::) * coeff
														X(perm(j), ::) -= X(perm(i), ::) * coeff       
											}

								}else{
               val pivot = A(perm(i), i)                 
                for (j <- i + 1 until A.rows) {
                  val coeff = A(perm(j),i) / pivot                 
                      A(perm(j), ::) -= A(perm(i), ::) * coeff
                      X(perm(j), ::) -= X(perm(i), ::) * coeff       
                }
                }
          }
						//val X1= DenseMatrix.zeros[ModRationalSpireExt](X.rows, X.cols)  
						val X1= new DenseMatrix[ModRationalSpireExt](X.rows, X.cols)
								for (i <- A.rows - 1 to (0, -1)) {
									X1(i, ::) := X(perm(i), ::)
											for (j <- i + 1 until A.rows) {               
											
												X1(i, ::) -= X1(j, ::) * A(perm(i), j)
											}
									
									X1(i, ::) /= A(perm(i), i)
								}

                
					
					//val X1= DenseMatrix.zeros[ModRationalSpireExt](X.rows, X.cols)  
					

		
        
      
      X:=X1.copy
     
     

      
		}
    */


		def LUSolve(X: DenseMatrix[ModRationalSpireExt], A: DenseMatrix[ModRationalSpireExt]) = {
			
      var perm = (0 until A.rows).toArray
					for (i <- 0 until A.rows-1) {
						val optPivot = (i until A.rows) find { p => A(perm(p),i) != ModRationalSpireExt.zero}
						val pivotRow = optPivot.getOrElse(throw new MatrixSingularException())
								val tmp = perm(i)
								perm(i) = perm(pivotRow)
								perm(pivotRow) = tmp
								val pivot = A(perm(i), i)
								for (j <- i + 1 until A.rows) {
									val coeff = A(perm(j),i) / pivot
											A(perm(j), ::) -= A(perm(i), ::) * coeff
											X(perm(j), ::) -= X(perm(i), ::) * coeff
								}
					}
			//val X1= DenseMatrix.zeros[ModRationalSpireExt](X.rows, X.cols)
			val X1= new DenseMatrix[ModRationalSpireExt](X.rows, X.cols)
					for (i <- A.rows - 1 to (0, -1)) {
						X1(i, ::) := X(perm(i), ::)
								for (j <- i + 1 until A.rows) {
									//X(perm(i), ::) -= X(perm(j), ::) * A(perm(i), j)
									X1(i, ::) -= X1(j, ::) * A(perm(i), j)
								}
						//X(perm(i), ::) /= A(perm(i), i)
						X1(i, ::) /= A(perm(i), i)
					}
			/* val X1= DenseMatrix.zeros[ModRationalSpireExt](X.rows, X.cols)
for( z<- 0 until A.rows){
X1(z,::) +=X(perm(z),::)
} */
      
			X:=X1.copy
		}

  
     
		override def apply(A: DenseMatrix[ModRationalSpireExt], V: DenseMatrix[ModRationalSpireExt]): DenseMatrix[ModRationalSpireExt] = {
			require(A.rows == V.rows, "Non-conformant matrix sizes")

			if (A.size == 0) {
				DenseMatrix.zeros[ModRationalSpireExt](0, 0)
			} else if (A.rows == A.cols) {
				/* val X = DenseMatrix.zeros[ModRationalSpireExt](V.rows, V.cols)
				val Y = DenseMatrix.zeros[ModRationalSpireExt](A.rows, A.cols) */
						// square: LUSolve
        val X = DenseMatrix.zeros[ModRationalSpireExt](V.rows, V.cols)
        val Y = DenseMatrix.zeros[ModRationalSpireExt](A.rows, A.cols) 
						X := V
						Y := A
             // LUSolve2(X, Y)
         
					LUSolve(X, Y)              
						X
           
			} else
				???
		}
	}
	implicit object implOpSolveMatrixBy_DMR_DVR_eq_DVR
	extends OpSolveMatrixBy.Impl2[DenseMatrix[ModRationalSpireExt], DenseVector[ModRationalSpireExt], DenseVector[ModRationalSpireExt]] {
		override def apply(a: DenseMatrix[ModRationalSpireExt], b: DenseVector[ModRationalSpireExt]): DenseVector[ModRationalSpireExt] = {
				val rv: DenseMatrix[ModRationalSpireExt] = a \ new DenseMatrix[ModRationalSpireExt](b.size, 1, b.data, b.offset, b.stride, true)
      
						new DenseVector[ModRationalSpireExt](rv.data)
		}
	}
	implicit def countFromTraverseModRationalSpireExt[T](implicit traverse: CanTraverseValues[T, ModRationalSpireExt]): countNonZero.Impl[T, Int] = {
			new countNonZero.Impl[T, Int] {
				def apply(t: T): Int = {
						var count: Int = 0
								traverse.traverse(t, new ValuesVisitor[ModRationalSpireExt] {
									def visit(a: ModRationalSpireExt) = { if (a != ModRationalSpireExt.zero) count += 1 }
									def zeros(count: Int, zeroValue: ModRationalSpireExt) {}
								})
								count
				}
			}
	}

}