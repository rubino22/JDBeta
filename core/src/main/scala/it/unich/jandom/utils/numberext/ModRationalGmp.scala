package it.unich.jandom.utils.numberext

import breeze.math.Field
import breeze.storage.Zero
import breeze.linalg._
import breeze.linalg.operators._
import org.netlib.util.intW
import it.unich.jandom.utils.breeze.countNonZero
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import org.gnu.gmp.swig.MPQ
import org.gnu.gmp.swig.MPZ

case class ModQ(n: MPQ) extends Ordered[ModQ] {
  val z= new MPQ();
  val num= new MPZ();
  val den= new MPZ();
  var num1= new MPZ();
  var den1= new MPZ();
  z.set_d(0);
  def *(b: ModQ) = {println("A "+n+" "+b.n);n.set_mul(n, b.n); ModQ(n)}
  def /(b: ModQ) = {b.n.get_den(den);println("B"+den.get_size+" "+b.n.get_d);if(n.cmp(z)==0 && b.n.cmp(z)==0){println("Bxx");n.set_d(0);ModQ(n)} else{println("BKKKK ->"+n.cmp(z)); n.set_div(n, b.n); ModQ(n)}}
  def +(b: ModQ) = {println("D"+n+" "+b.n);n.set_add(n, b.n); ModQ(n)}
  def -(b: ModQ) = {println("E"+n+" "+b.n);n.set_sub(n, b.n); ModQ(n)}
  def unary_- = {println("F"+n); n.set_neg(n);  ModQ(n)}  
  def pow(b: ModQ) = {
    var res = new MPQ()
    res.set_d(1)
    for (i <- 0 until b.n.get_d.toInt) {res.set_mul(res, n)} 
    ModQ(res)
  }
  def compare(b: ModQ) = n.cmp(b.n) 
  def isInfinite = {  println("X");den.set_d(0); num.set_d(0); z.set_den(den); z.set_num(num); println("1n="+n+" "+z); if(z.equal(n)) true else false}
  def isPosInfinity = {println("Y"); den.set_d(1); num.set_d(0); z.set_den(den); z.set_num(num); n.get_den(num1); n.get_num(den1); println("2n="+n +" "+z);if(num.equals(num1) && den.equals(den1)){ print("true");true} else {print("false");false}}
  def isNegInfinity = { println("Z");den.set_d(0); num.set_d(-1); z.set_den(den); z.set_num(num);println("3n="+n+" "+z); if(z.equal(n)) true else false}
  def isInfinity = { isPosInfinity || isNegInfinity}
 
  def max(b: ModQ) = { n.cmp(b.n)  match {
    case 1 => ModQ(n)
    case _ => ModQ(b.n)
      }
    }
  def min(b: ModQ) = { n.cmp(b.n)  match {
    case -1 => ModQ(n)
    case _ => ModQ(b.n)
      }
    }
  override def >(b: ModQ) = { n.cmp(b.n)  match {
    case 1 => true
    case _ => false
      }
    }
 override def >=(b: ModQ) = { n.cmp(b.n)  match {
    case 1 => true
    case 0 => true
    case _ => false
      }
    }
 
 override def <(b: ModQ) = { n.cmp(b.n)  match {
    case 1 => false
    case _ => true
      }
    }
 override def <=(b: ModQ) = { n.cmp(b.n)  match {
    case 1 => false
    case 0 => true
    case _ => true
      }
    } 
  def !=(b: ModQ) = { n.cmp(b.n)  match {    
    case 0 => false
    case _ => true
      }
    } 
   def ==(b: ModQ) = { n.cmp(b.n)  match {    
    case 0 => true
    case _ => false
      }
    } 
  def toDouble: Double = n.get_d

}

object ModQ { outer =>  

  val zero = ModQ(0.0)

  val one = ModQ(1.0)
 
   //val five = ModQ(5.3)
  //val NegativeInfinity = ModQ(-1/0)
  val NegativeInfinity = ModQ(Double.NegativeInfinity)

  val PositiveInfinity = ModQ(Double.PositiveInfinity)

  def apply(d: Double): ModQ = {
    val r= new MPQ();
    val num= new MPZ();
    val den= new MPZ();
  if(!d.isInfinite()){r.set_d(d); ModQ(r)} else{if(d.isNegInfinity) {den.set_d(0); num.set_d(-1);r.set_den(den); r.set_num(num);    ModQ(r)} else {den.set_d(1); num.set_d(0);r.set_den(den); r.set_num(num);  ModQ(r)}} 
  }

  implicit object scalar extends Field[ModQ] {
    def zero = outer.zero

    def one = outer.one

    def ==(a: ModQ, b:ModQ) = a == b

    def !=(a: ModQ, b:ModQ) = a != b

    def +(a: ModQ, b: ModQ) = a + b

    def -(a: ModQ, b: ModQ) = a - b

    def *(a: ModQ, b: ModQ) = a * b

    def /(a: ModQ, b: ModQ) = a / b

    def %(a: ModQ, b: ModQ) = outer.zero

    def pow(a: ModQ, b: ModQ) = a pow b
  
    def >(a: ModQ, b: ModQ) = a > b
    
    def >=(a: ModQ, b: ModQ) = a >= b
    
    def <(a: ModQ, b: ModQ) = a < b
    
    def <=(a: ModQ, b: ModQ) = a <= b
    
    implicit val normImpl: norm.Impl[ModQ, Double] = new norm.Impl[ModQ, Double] {
      def apply(v: ModQ): Double =v.n.get_d
    }

  }

  

  trait ModQNumeric extends Numeric[ModQ] {
    def plus(x: ModQ, y: ModQ): ModQ = x + y
    def minus(x: ModQ, y: ModQ): ModQ = x - y
    def times(x: ModQ, y: ModQ): ModQ = x * y
    def negate(x: ModQ): ModQ = -x
    def fromInt(x: Int): ModQ = ModQ(x.toDouble )
    def toInt(x: ModQ): Int = x.n.get_d().toInt
    def toLong(x: ModQ): Long = x.n.get_d().toLong
    def toFloat(x: ModQ): Float = x.n.get_d().toFloat
    def toDouble(x: ModQ): Double = x.n.get_d()
  }

  trait ModQFractional extends ModQNumeric with Fractional[ModQ] {
    def div(x: ModQ, y: ModQ): ModQ = x / y
  }

  trait ModQOrdering extends Ordering[ModQ] {
    override def compare(a : ModQ, b : ModQ) = a.n.cmp(b.n)
  }

  implicit object ModQFractional extends ModQFractional with ModQOrdering

  implicit object MulMM extends OpMulMatrix.Impl2[ModQ,ModQ,ModQ]
  { def apply(a : ModQ, b : ModQ) = a * b}

  implicit object MulDM extends OpDiv.Impl2[Double,ModQ,ModQ]
  { def apply(a : Double, b : ModQ) = ModQ(a) * b }

  implicit object ModQZero extends Zero[ModQ] {
	  val zero = outer.zero
  }
  implicit def dv_s_Op_ModQ_OpMulMatrix: OpMulMatrix.Impl2[DenseVector[ModQ], ModQ, DenseVector[ModQ]] =
		  new OpMulMatrix.Impl2[DenseVector[ModQ], ModQ, DenseVector[ModQ]] {
		  def apply(a: DenseVector[ModQ], b: ModQ): DenseVector[ModQ] = {
				  val ad = a.data
						  var aoff = a.offset
						  val result = DenseVector.zeros[ModQ](a.length)
						  val rd = result.data
						  var i = 0
						  while (i < a.length) {
							  rd(i) = ad(aoff) * b
									  aoff += a.stride
									  i += 1
						  }
				  result
		  }
		  implicitly[BinaryRegistry[Vector[ModQ], ModQ, OpMulMatrix.type, Vector[ModQ]]].register(this)
	  }
  
	  implicit object implOpSolveMatrixBy_DRR_DRR_eq_DRR
	  extends OpSolveMatrixBy.Impl2[DenseMatrix[ModQ], DenseMatrix[ModQ], DenseMatrix[ModQ]] {
		  def LUSolve(X: DenseMatrix[ModQ], A: DenseMatrix[ModQ]) = {
        
        var perm = (0 until A.rows).toArray
          for (i <- 0 until A.rows) {
             println(">"+A);
						  val optPivot = (i until A.rows) find { p => A(perm(p), perm(i)) != ModQ.zero }
              println(optPivot);
						 val pivotRow = optPivot.getOrElse(throw new MatrixSingularException())
             //val pivotRow =1
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
		  override def apply(A: DenseMatrix[ModQ], V: DenseMatrix[ModQ]): DenseMatrix[ModQ] = {
			  require(A.rows == V.rows, "Non-conformant matrix sizes")
       
			  if (A.size == 0) {
				  DenseMatrix.zeros[ModQ](0, 0)
			  } else if (A.rows == A.cols) {
				  val X = DenseMatrix.zeros[ModQ](V.rows, V.cols)
				  val Y = DenseMatrix.zeros[ModQ](A.rows, A.cols)
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
	  extends OpSolveMatrixBy.Impl2[DenseMatrix[ModQ], DenseVector[ModQ], DenseVector[ModQ]] {
		  override def apply(a: DenseMatrix[ModQ], b: DenseVector[ModQ]): DenseVector[ModQ] = {
				  val rv: DenseMatrix[ModQ] = a \ new DenseMatrix[ModQ](b.size, 1, b.data, b.offset, b.stride, true)
						  new DenseVector[ModQ](rv.data)
		  }
	  }
	  implicit def countFromTraverseModQ[T](implicit traverse: CanTraverseValues[T, ModQ]): countNonZero.Impl[T, Int] = {
			  new countNonZero.Impl[T, Int] {
				  def apply(t: T): Int = {
						  var count: Int = 0
								  traverse.traverse(t, new ValuesVisitor[ModQ] {
									  def visit(a: ModQ) = { if (a != ModQ.zero) count += 1 }
									  def zeros(count: Int, zeroValue: ModQ) {}
								  })
								  count
				  }
			  }
}

}
