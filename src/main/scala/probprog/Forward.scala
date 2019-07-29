package probprog

import spire.implicits._
import spire.math._
import spire.algebra._
import cats._
import cats.implicits._

case class Dual[A: Numeric](real: A, eps: Double) { self =>
  def map[B: Numeric](f: A => B): Dual[B] =
    self.flatMap(a => Dual.pure(f(a)))

  def flatMap[B: Numeric](f: A => Dual[B]): Dual[B] = {
    val x = f(real)
    val nextEps = self.eps * x.eps
    Dual(x.real, nextEps)
  }

  def sin(implicit ev: A =:= Double) =
    self.flatMap(x => Dual(math.sin(x), math.cos(x)))

  def cos(implicit ev: A =:= Double) =
    self.flatMap(x => Dual(math.cos(x), -math.sin(x)))

  def exp(implicit ev: A =:= Double) =
    self.flatMap(x => Dual(math.exp(x), math.exp(x)))

  def log(implicit ev: A =:= Double) =
    self.flatMap(x => Dual(math.log(real), 1 / real))

  def pow(a: A)(implicit ev: A =:= Double) =
    self.flatMap(x => Dual(math.pow(x, a), a * math.pow(x, a - 1)))

  def plus(y: Dual[A]) =
    Dual[A](self.real + y.real, self.eps + y.eps)

  def times(y: Dual[A])(implicit ev: ConvertableFrom[A]) =
    Dual[A](self.real * y.real, self.eps * ev.toType[Double](y.real) + y.eps * ev.toType[Double](self.real))

  def div(y: Dual[A])(implicit ev: ConvertableFrom[A]) =
    Dual[A](self.real / y.real, (self.eps * ev.toType[Double](y.real) - ev.toType[Double](self.real) * y.eps) / (ev.toType[Double](y.real) * ev.toType[Double](y.real)))

  def negate: Dual[A] =
    Dual[A](-self.real, -self.eps)

   def gradient: Double = eps
}

object Dual {
  def pure[A: Numeric](a: A): Dual[A] = {
    Dual(a, 1.0)
  }

  def const[A: Numeric](a: A): Dual[A] = {
    Dual(a, 0.0)
  }
}
