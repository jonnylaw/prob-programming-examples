package probprog

import spire.implicits._
import spire.math._
import org.scalatest._
import prop._
import org.scalactic.Equality
import org.scalacheck.Gen
import probprog._

class ForwardModeAd
    extends PropSpec
    with GeneratorDrivenPropertyChecks
    with Matchers {

  val smallDouble = Gen.choose(2.0, 10.0)

  property("Forward Mode AD") {
    forAll(smallDouble) { (a: Double) =>
      def f(x: Dual[Double]): Dual[Double] = x times Dual.const(5)
        assert(f(Dual.pure(a)).gradient === 5.0 +- 0.1)
    }
  }

  property("Derivative of sin") {
    forAll(smallDouble) { (a: Double) =>
      def f(x: Dual[Double]): Dual[Double] = x.sin
      assert(f(Dual.pure(a)).gradient === math.cos(a) +- 0.1)
    }
  }

  property("Forward: AD composite function") {
    forAll(smallDouble) { (a: Double) =>
      // f(x) = sin(cos(x)), f'(x) = -sin(x)cos(cos(x))
      val f = (x: Dual[Double]) => x.cos.sin
      val derivative = -math.sin(a) * math.cos(math.cos(a))
      assert(f(Dual.pure(a)).gradient === derivative +- 0.1)
    }
  }

  property("Forward mode is equivalent to reverse") {
    forAll(smallDouble) { (a: Double) =>
      def f(x: Dual[Double]): Dual[Double] =
        x.cos plus x.sin
      def f1(x: Reverse[Double]): Reverse[Double] =
        x.cos plus x.sin
      val forward = f(Dual.pure(a))
      val reverse = f1(Reverse.pure(a))
      val analytic = -sin(a) + cos(a)

      assert(forward.gradient === analytic +- 0.1)
      assert(reverse.gradient === forward.gradient +- 0.1)
    }
  }

  property("Forward: Derivative of log(sin(x)) = cos(x) / sin(x)") {
    forAll(smallDouble) { (a: Double) =>
      def f(x: Dual[Double]): Dual[Double] =
        x.sin.log

      def analytic = cos(a) / sin(a)
      assert(f(Dual.pure(a)).gradient === analytic +- 0.1)
    }
  }

  property("Forward: Derivative of exp(-x^2) is -2x exp(-x^2)") {
    forAll(smallDouble) { (a: Double) =>
      def f(x: Dual[Double]): Dual[Double] =
        x.pow(2).negate.exp

      def analytic = - 2 * a * math.exp(- a * a)
      assert(f(Dual.pure(a)).gradient === analytic +- 0.1)
    }
  }

  property("Derivative of log(log(x)) is 1 / xlog(x)") {
    forAll(smallDouble) { (a: Double) =>
      def f(x: Dual[Double]): Dual[Double] =
        x.log.log

      def analytic = 1 / (a * math.log(a))
      assert(f(Dual.pure(a)).gradient === analytic +- 0.1)
    }
  }
}
