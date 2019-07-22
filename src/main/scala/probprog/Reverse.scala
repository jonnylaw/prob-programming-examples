package probprog

import spire.implicits._
import spire.math._
import spire.algebra._

final case class Reverse[A](real: A, continuation: Double => Double) { self =>
  def flatMap[B](f: A => Reverse[B]): Reverse[B] = {
    val next = f(real)
    Reverse(next.real, (d: Double) => next.continuation(d) * self.continuation(d))
  }

  def map[B](f: A => B): Reverse[B] =
    Reverse(f(real), (d: Double) => self.continuation(d))

  def gradient: Double =
    self.continuation(1.0)

  // primative functions
  def sin(implicit ev: A =:= Double): Reverse[Double] =
    self.flatMap(x => Reverse(math.sin(x), (d: Double) => math.cos(x) * d))

  def cos(implicit ev: A =:= Double): Reverse[Double] =
    self.flatMap(x => Reverse(math.cos(x), (d: Double) => -math.sin(x) * d))

  def exp(implicit ev: A =:= Double): Reverse[Double] =
    self.flatMap(x => Reverse(math.exp(x), (d: Double) => math.exp(x) * d))

  def log(implicit ev: A =:= Double): Reverse[Double] =
    self.flatMap(x => Reverse(math.log(x), (d: Double) => (x * d) / math.log(x)))

  def plus(y: Reverse[Double])(implicit ev: A =:= Double) =
    Reverse[Double](self.real + y.real,
                    (d: Double) => self.continuation(d) + y.continuation(d))

  def times(y: Reverse[Double])(implicit ev: A =:= Double) =
    Reverse[Double](self.real * y.real,
                    (d: Double) => self.continuation(d) * y.real + y.continuation(d) * self.real)

  def div(y: Reverse[Double])(implicit ev: A =:= Double) =
    Reverse[Double](self.real / y.real,
                    (d: Double) => (self.continuation(d) * y.real - self.real * y.continuation(d)) / (y.real * y.real))

  def negate(implicit ev: A =:= Double): Reverse[Double] =
    Reverse[Double](-self.real, (d: Double) => -self.continuation(d))
}

object Reverse {
  // lift a value into the context of the monad
  def pure(x: Double): Reverse[Double] =
    Reverse(x, d => 1.0)

  def const(x: Double): Reverse[Double] =
    Reverse(x, d => 0.0)

  implicit val dualNumeric = new Numeric[Dual[Double]] {
    def plus(x: Dual[Double], y: Dual[Double]) =
      x plus y
    def times(x: Dual[Double], y: Dual[Double]) =
      x times y
    def div(x: Dual[Double], y: Dual[Double]) =
      x div y
    def negate(x: Dual[Double]): Dual[Double] =
      x.negate

    // Members declared in algebra.ring.AdditiveMonoid
    def zero: Dual[Double] = Dual[Double](0.0, 0.0)
    // Members declared in spire.math.ConvertableFrom
    def toAlgebraic(a: Dual[Double]): Algebraic = ???
    def toBigDecimal(a: Dual[Double]): BigDecimal = ???
    def toBigInt(a: Dual[Double]): BigInt = ???
    def toByte(a: Dual[Double]): Byte = ???
    def toFloat(a: Dual[Double]): Float = ???
    def toInt(a: Dual[Double]): Int = ???
    def toLong(a: Dual[Double]): Long = ???
    def toNumber(a: Dual[Double]): Number = ???
    def toRational(a: Dual[Double]): Rational = ???
    def toShort(a: Dual[Double]): Short = ???
    def toString(a: Dual[Double]): String = ???
    def toType[B](a: Dual[Double])(implicit evidence$17: ConvertableTo[B]): B = ???

    // Members declared in ConvertableTo
    def fromAlgebraic(n: Algebraic): Dual[Double] = ???
    def fromBigDecimal(n: BigDecimal): Dual[Double] = ???
    def fromByte(n: Byte): Dual[Double] = ???
    def fromDouble(n: Double): Dual[Double] = ???
    def fromFloat(n: Float): Dual[Double] = ???
    def fromLong(n: Long): Dual[Double] = ???
    def fromRational(n: Rational): Dual[Double] = ???
    def fromReal(n: Real): Dual[Double] = ???
    def fromShort(n: Short): Dual[Double] = ???
    def fromType[B](b: B)(implicit evidence$1: ConvertableFrom[B]): Dual[Double] = ???

    // Members declared in spire.algebra.IsReal
    def ceil(a: Dual[Double]): Dual[Double] = ???
    def floor(a: Dual[Double]): Dual[Double] = ???
    def isWhole(a: Dual[Double]): Boolean = ???
    def round(a: Dual[Double]): Dual[Double] = ???
    def toDouble(a: Dual[Double]): Double = ???
    def toReal(a: Dual[Double]): Real = ???

    // Members declared in algebra.ring.MultiplicativeMonoid
    def one: Dual[Double] = Dual[Double](1.0, 0.0)

    // Members declared in spire.algebra.NRoot
    def fpow(a: Dual[Double],b: Dual[Double]): Dual[Double] = ???
    def nroot(a: Dual[Double],n: Int): Dual[Double] = ???
      // Members declared in cats.kernel.Order
    def compare(x: Dual[Double],y: Dual[Double]): Int = ???
      // Members declared in spire.algebra.Signed
    def abs(a: Dual[Double]): Dual[Double] = ???
    def signum(a: Dual[Double]): Int = ???
  }
}
