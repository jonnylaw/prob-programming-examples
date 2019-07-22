package probprog

import com.stripe.rainier.compute._
import com.stripe.rainier.core._
import com.stripe.rainier.sampler._
import java.io.{File, PrintWriter}
import scala.concurrent.ExecutionContext.Implicits.global
import cats.effect.IO

object Lm extends App {
  implicit val rng = ScalaRNG(2)

  val n = 100
  val x = Vector.fill(n)(rng.standardNormal)

  val b0 = 4.0
  val b1 = -1.5

  val y = x map (xi => b0 + b1 * xi + rng.standardNormal * 0.5)

  val sims = x zip y

  val pw = new PrintWriter(new File("data/lm_sims.csv"))
  pw.write(List("x", "y").mkString(",") ++ "\n")
  pw.write(sims.map { case (x, y) => s"$x, $y" }.mkString("\n"))
  pw.close()

  def linearModel(
    priorMeanAlpha: Real,
    priorSdAlpha: Real,
    priorMeanBeta: Real,
    priorSdBeta: Real,
    sigma: Real,
    data: Vector[(Double, Double)]): RandomVariable[Map[String, Real]] = for {
    alpha <- Normal(priorMeanAlpha, priorSdAlpha).param
    beta <- Normal(priorMeanBeta, priorSdBeta).param
    _ <- Predictor[Double].from { x =>
      Normal(alpha + beta * x, sigma)
    }
    .fit(data)
  } yield Map("alpha" -> alpha, "beta" -> beta, "sigma" -> sigma)

  val model = for {
    sigma <- Exponential(3.0).param
    params <- linearModel(0.0, 10.0, 0.0, 10.0, sigma, sims)
  } yield params

  // write two parallel chains
  implicit val contextShift = IO.contextShift(global)
  ParallelChains.run(model, 2, "data/lm").
    unsafeRunSync()
}
