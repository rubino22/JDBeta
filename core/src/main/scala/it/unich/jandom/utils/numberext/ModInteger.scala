package it.unich.jandom.utils.numberext

import breeze.math.Field
import breeze.storage.Zero
import breeze.linalg._
import breeze.linalg.operators._
import org.netlib.util.intW
import it.unich.jandom.utils.breeze.countNonZero
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor

case class ModZ(n: Int) extends Ordered[ModZ] {
  val p = 3
  def *(b: ModZ) = ModZ((n*b.n) % p)
  def /(b: ModZ) = ModZ((n/b.n) % p)
  def +(b: ModZ) = ModZ((n+b.n) % p)
  def -(b: ModZ) = ModZ((n-b.n) % p)
  def pow(b: ModZ) = {
    var res = 1
    for (i <- 0 until b.n) res *= n
    ModZ(res % p)
  }
  def compare(b: ModZ) = n - b.n
  def isInfinite = false
  def isPosInfinity = n == p-1
  def isNegInfinity = n == 0
  def isInfinity = isPosInfinity || isNegInfinity
  def max(b: ModZ) = ModZ(n max b.n)
  def min(b: ModZ) = ModZ(n min b.n)
  def toDouble: Double = n.toDouble

}

object ModZ { outer =>
  val p = 3

  val zero = ModZ(0)

  val one = ModZ(1)

  val NegativeInfinity = zero

  val PositiveInfinity = ModZ(p - 1)

  def apply(d: Double): ModZ = ModZ( d.toInt % p )

  implicit object scalar extends Field[ModZ] {
    def zero = outer.zero

    def one = outer.one

    def ==(a: ModZ, b:ModZ) = a == b

    def !=(a: ModZ, b:ModZ) = a != b

    def +(a: ModZ, b: ModZ) = a + b

    def -(a: ModZ, b: ModZ) = a - b

    def *(a: ModZ, b: ModZ) = a * b

    def /(a: ModZ, b: ModZ) = a / b

    def %(a: ModZ, b: ModZ) = outer.zero

    def pow(a: ModZ, b: ModZ) = a pow b

    implicit val normImpl: norm.Impl[ModZ, Double] = new norm.Impl[ModZ, Double] {
      def apply(v: ModZ): Double = v.n.toDouble
    }

  }

  

  trait ModZNumeric extends Numeric[ModZ] {
    def plus(x: ModZ, y: ModZ): ModZ = x + y
    def minus(x: ModZ, y: ModZ): ModZ = x - y
    def times(x: ModZ, y: ModZ): ModZ = x * y
    def negate(x: ModZ): ModZ = -x
    def fromInt(x: Int): ModZ = ModZ(x % p)
    def toInt(x: ModZ): Int = x.n
    def toLong(x: ModZ): Long = x.n.toLong
    def toFloat(x: ModZ): Float = x.n.toFloat
    def toDouble(x: ModZ): Double = x.n.toDouble
  }

  trait ModZFractional extends ModZNumeric with Fractional[ModZ] {
    def div(x: ModZ, y: ModZ): ModZ = x / y
  }

  trait ModZOrdering extends Ordering[ModZ] {
    override def compare(a : ModZ, b : ModZ) = a.n - b.n
  }

  implicit object ModZFractional extends ModZFractional with ModZOrdering

  implicit object MulMM extends OpMulMatrix.Impl2[ModZ,ModZ,ModZ]
  { def apply(a : ModZ, b : ModZ) = a * b}

  implicit object MulDM extends OpDiv.Impl2[Double,ModZ,ModZ]
  { def apply(a : Double, b : ModZ) = ModZ(a.toInt % p) * b }

  implicit object ModZZero extends Zero[ModZ] {
	  val zero = outer.zero
  }
  implicit def dv_s_Op_ModZ_OpMulMatrix: OpMulMatrix.Impl2[DenseVector[ModZ], ModZ, DenseVector[ModZ]] =
		  new OpMulMatrix.Impl2[DenseVector[ModZ], ModZ, DenseVector[ModZ]] {
		  def apply(a: DenseVector[ModZ], b: ModZ): DenseVector[ModZ] = {
				  val ad = a.data
						  var aoff = a.offset
						  val result = DenseVector.zeros[ModZ](a.length)
						  val rd = result.data
						  var i = 0
						  while (i < a.length) {
							  rd(i) = ad(aoff) * b
									  aoff += a.stride
									  i += 1
						  }
				  result
		  }
		  implicitly[BinaryRegistry[Vector[ModZ], ModZ, OpMulMatrix.type, Vector[ModZ]]].register(this)
	  }
	  implicit object implOpSolveMatrixBy_DRR_DRR_eq_DRR
	  extends OpSolveMatrixBy.Impl2[DenseMatrix[ModZ], DenseMatrix[ModZ], DenseMatrix[ModZ]] {
		  def LUSolve(X: DenseMatrix[ModZ], A: DenseMatrix[ModZ]) = {
			  var perm = (0 until A.rows).toArray
					  for (i <- 0 until A.rows) {
						  val optPivot = (i until A.rows) find { p => A(perm(p), perm(i)) != ModZ.zero }
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
		  override def apply(A: DenseMatrix[ModZ], V: DenseMatrix[ModZ]): DenseMatrix[ModZ] = {
			  require(A.rows == V.rows, "Non-conformant matrix sizes")
			  if (A.size == 0) {
				  DenseMatrix.zeros[ModZ](0, 0)
			  } else if (A.rows == A.cols) {
				  val X = DenseMatrix.zeros[ModZ](V.rows, V.cols)
						  val Y = DenseMatrix.zeros[ModZ](A.rows, A.cols)
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
	  extends OpSolveMatrixBy.Impl2[DenseMatrix[ModZ], DenseVector[ModZ], DenseVector[ModZ]] {
		  override def apply(a: DenseMatrix[ModZ], b: DenseVector[ModZ]): DenseVector[ModZ] = {
				  val rv: DenseMatrix[ModZ] = a \ new DenseMatrix[ModZ](b.size, 1, b.data, b.offset, b.stride, true)
						  new DenseVector[ModZ](rv.data)
		  }
	  }
	  implicit def countFromTraverseModZ[T](implicit traverse: CanTraverseValues[T, ModZ]): countNonZero.Impl[T, Int] = {
			  new countNonZero.Impl[T, Int] {
				  def apply(t: T): Int = {
						  var count: Int = 0
								  traverse.traverse(t, new ValuesVisitor[ModZ] {
									  def visit(a: ModZ) = { if (a != ModZ.zero) count += 1 }
									  def zeros(count: Int, zeroValue: ModZ) {}
								  })
								  count
				  }
			  }
}

}
