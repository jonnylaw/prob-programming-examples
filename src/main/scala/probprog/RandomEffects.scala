package probprog

import com.stripe.rainier.compute._
import com.stripe.rainier.core._
import com.stripe.rainier.sampler._
import java.io.{File, PrintWriter}
import scala.concurrent.ExecutionContext.Implicits.global
import cats.effect._

object RandomEffects extends App {
  implicit val rng = ScalaRNG(2)

  // simulate some random data
  val alphaC = 3.0
  val betaC = 2.5
  val sigmaA = 1.0
  val sigmaB = 0.5
  val sigma = 0.5

  case class Observation(
    id: Int,
    x: Double,
    y: Double
  )

  def gaussian(mean: Double, sd: Double) = {
    scala.util.Random.nextGaussian() * sd + mean
  }

  def lm(alpha: Double, beta: Double, x: Double, sigma: Double) = {
    gaussian(alpha + beta * x, sigma)
  }

  def simulate(k: Int, n: Int) = {
    val x = Vector.fill(n)(gaussian(0.0, 0.2))
    Vector.range(0, k, 1).
      flatMap { i =>
        val alphai = gaussian(alphaC, sigmaA)
        val betai = gaussian(betaC, sigmaB)
        (0 until n).map{ j =>
          Observation(i, x(j), lm(alphai, betai, x(j), sigma))
          }
        }
      }

  val observations = simulate(20, 30)

  val pw = new PrintWriter(new File("data/random_effects_sims.csv"))
  pw.write(List("id", "x", "y").mkString(",") ++ "\n")
  pw.write(observations.map { x => s"${x.id}, ${x.x}, ${x.y}" }.mkString("\n"))
  pw.close()

  // prior for the shared parameters
  val prior = for {
    alphaC <- Normal(0.0, 1000).param
    alphaSigma <- Gamma(0.001, 1000).param
    betaC <- Normal(0.0, 1000).param
    betaSigma <- Gamma(0.001, 1000).param
    sigma <- Exponential(3).param
  } yield (alphaC, alphaSigma, betaC, betaSigma, sigma)

  // nested linear regression model
  val model: RandomVariable[Map[String, Real]] = for {
    (ac, sa, bc, sb, sigma) <- prior
    _ <- RandomVariable.traverse(
      observations.
        groupBy(_.id).
        map { case (id, obs) =>
          Lm.linearModel(ac, sa, bc, sb,
                         sigma, obs.map(ys => (ys.x, ys.y)))
        }.toVector)
  } yield Map("alphaC" -> ac, "sigmaA" -> sa,
              "betaC" -> bc, "sigmaB" -> sb, "sigma" -> sigma)

  // write two parallel chains
  implicit val contextShift = IO.contextShift(global)
  ParallelChains.run(model, 2, "data/random_effects").unsafeRunSync()
}
