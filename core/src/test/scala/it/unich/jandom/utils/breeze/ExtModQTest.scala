package it.unich.jandom.utils.breeze

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import breeze.linalg.{DenseMatrix => BDM, CSCMatrix => BSM}
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary
import org.scalatest.FunSpec
import org.scalacheck.Gen
import it.unich.jandom.utils.numberext.ModRationalGmpExt
import org.scalatest.prop.Checkers
import it.unich.jandom.utils.numberext.GenericNumberExt._
import org.scalatest.MustMatchers
import org.gnu.gmp.swig.MPQ
import breeze.linalg.DenseMatrix

/**
 *
 * @author Marco Rubino 
 */
class ExtModQTest   extends FunSpec with GeneratorDrivenPropertyChecks with Checkers {
	import ModRationalGmpExt._
  
	val rationalGenerator = for {
		numer <- Gen.choose(-1000,1000)
	} yield ModRationalGmpExt(numer)
  
	implicit val ModQExtArbitrary = Arbitrary( rationalGenerator )
  
	describe("Sum") {
		it("is commutative") {
			forAll { (r1: ModRationalGmpExt, r2: ModRationalGmpExt) =>
			assert(r1 + r2 == r1 + r2)     
      
			}

		
  }
  
		
		describe("equality") {
			var r1 = ModRationalGmpExt(2.3)
					var r2 =  ModRationalGmpExt(2.300004)
					it("is  r2 > r1") {assert(r2 > r1, "r2 > r1")}
			it("is  r1 < r2") {assert(r1 < r2, "r1 < r2")}
			it("is  r2 >= r1") {assert(r2 >= r1, "r2 >= r1")}    
			it("is  r1 <= r2") {assert(r1 <= r2, "r1 <= r2")}
			var r3=ModRationalGmpExt(2.3)
					var r4=ModRationalGmpExt(2.3)
					it("is  r3 == r4") {assert(r3 == r4, "r3 == r4")}
		}
		describe("equality on infinities") {
			it("is +Inf = +Inf") { assert(PositiveInfinity == PositiveInfinity, "+Inf = +Inf")}
			it("is -Inf = -Inf") { assert(NegativeInfinity == NegativeInfinity, "-Inf = -Inf")}
			it("is +Inf != -Inf") { assert(PositiveInfinity != NegativeInfinity, "+Inf != -Inf")}
			it("is NegativeInfinity2") {assert(NegativeInfinity != PositiveInfinity, "-Inf != +Inf")}
			it("is -Inf != +Inf") { assert(NegativeInfinity != NaN, "-Inf != NaN")}
			it("is +Inf != NaN") {assert(PositiveInfinity!= NaN, "+Inf != NaN")}
			it("is NaN = NaN") { assert(NaN == NaN, "NaN = NaN")}
			it("is NaN != -Inf") {assert(NaN != NegativeInfinity, "NaN != -Inf")}
			it("is NaN != +Inf") { assert(NaN != PositiveInfinity, "NaN != +Inf")   } 
			it("is NaN != 2") { assert(NaN != ModRationalGmpExt(2), "NaN != 2")}
		}


		describe("equality on regular numbers") {
			val n1 = ModRationalGmpExt(2.5)
					val n2 = ModRationalGmpExt(2.5)
					val n3 = ModRationalGmpExt(3.2)
					assert(n1==n1)
					assert(n1==n2)
					assert(n1!=n3)
					assert(n2==n1)
					assert(n2==n2)
					assert(n2!=n3)
					assert(n3!=n1)
					assert(n3!=n2)
					assert(n3==n3)
		}

	 describe("Test Infinity") {
      it("Test Infinity") {
        val pos = ModRationalGmpExt(Double.PositiveInfinity)              
        assert( pos.isPosInfinity == true, "true")
        assert( pos.isNegInfinity == false, "false")
        assert( pos.isInfinity == true, "true")
        val neg = ModRationalGmpExt(Double.NegativeInfinity)              
        assert( neg.isPosInfinity == false, "true")
        assert( neg.isNegInfinity == true, "false")
        assert( neg.isInfinity == true, "true")
      }
    }
		describe("sum on infinities") {
			assertResult (PositiveInfinity) { ModRationalGmpExt(3) + PositiveInfinity }    
			assertResult (PositiveInfinity) { PositiveInfinity + ModRationalGmpExt(3)  }
			assertResult (PositiveInfinity) { PositiveInfinity + PositiveInfinity  }
			assertResult (NegativeInfinity) { ModRationalGmpExt(3) + NegativeInfinity }
			assertResult (NegativeInfinity) { NegativeInfinity + ModRationalGmpExt(3)  }
			assertResult (NegativeInfinity) { NegativeInfinity + NegativeInfinity }
			assertResult (NaN) { NaN + ModRationalGmpExt(3) }
			assertResult (NaN) { ModRationalGmpExt(3) + NaN }
			assertResult (NaN) { PositiveInfinity + NegativeInfinity }
			assertResult (NaN) { NegativeInfinity + PositiveInfinity }
			assertResult (NaN) { NaN + NaN }
		}

		describe("difference on infinities") {
			assertResult (NegativeInfinity) { ModRationalGmpExt(3) - PositiveInfinity }
			assertResult (PositiveInfinity) { PositiveInfinity - ModRationalGmpExt(3)  }
			assertResult (NaN) { PositiveInfinity - PositiveInfinity  }
			assertResult (PositiveInfinity) { ModRationalGmpExt(3) - NegativeInfinity }
			assertResult (NegativeInfinity) { NegativeInfinity - ModRationalGmpExt(3)  }
			assertResult (NaN) { NegativeInfinity - NegativeInfinity }
			assertResult (NaN) { NaN - ModRationalGmpExt(3) }
			assertResult (NaN) { ModRationalGmpExt(3) - NaN }
			assertResult (PositiveInfinity) { PositiveInfinity - NegativeInfinity }
			assertResult (NegativeInfinity) { NegativeInfinity - PositiveInfinity }
			assertResult (NaN) { NaN - NaN }
		}   

		describe ("toString method") {
			check( (w:Int) => ModRationalGmpExt(w).toString == w.toString )
			assertResult ("+Inf") { PositiveInfinity.toString }
			assertResult ("-Inf") { NegativeInfinity.toString }
			assertResult ("NaN") { NaN.toString }
		}
    
		    
	}
}