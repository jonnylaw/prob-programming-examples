import org.scalatest._
import prop._
import scala.math._
import org.scalactic.Equality
import org.scalacheck.Gen
import probprog._

class ReverseModeAd
    extends PropSpec
    with GeneratorDrivenPropertyChecks
    with Matchers {

  val smallDouble = Gen.choose(2.0, 10.0)

  property("Reverse Mode AD") {
    forAll(smallDouble) { (a: Double) =>
      // f(x) = sin^2(x), f'(x) = 2cos(x)sin(x)
      val f = (x: Reverse[Double]) => x.sin times x.sin
      val derivative = 2 * math.sin(a) * math.cos(a)
      assert(f(Reverse.pure(a)).gradient === derivative +- 0.1)
    }
  }

  property("Reverse mode AD composite function") {
    forAll(smallDouble) { (a: Double) =>
      // f(x) = sin(cos(x)), f'(x) = -sin(x)cos(cos(x))
      val f = (x: Reverse[Double]) => x.cos.sin
      val derivative = -math.sin(a) * math.cos(math.cos(a))
      assert(f(Reverse.pure(a)).gradient === derivative +- 0.1)
    }
  }

  property("Reverse: Derivative of sin(x) + 5x is cos(x) + 5") {
    forAll(smallDouble) { (a: Double) =>
      def f1(x: Reverse[Double]): Reverse[Double] =  x.sin.plus(x.times(Reverse.const(5)))
      val reverse = f1(Reverse.pure(a))
      val analytic = 5 + cos(a)

      assert(reverse.gradient === analytic +- 0.1)
    }
  }

  property("Reverse: Derivative of Gaussian") {
    forAll(smallDouble) { (a: Double) =>

    }
  }
}


