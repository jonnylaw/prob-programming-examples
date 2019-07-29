package probprog

import com.stripe.rainier.compute._

import com.stripe.rainier.core._
import com.stripe.rainier.sampler._
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.{File, PrintWriter}
import cats.effect._

object SimulateMixtureModel extends App {
  implicit val rng = ScalaRNG(2)
  implicit val evaluator: Evaluator = new Evaluator(Map.empty)
  case class Parameters(thetas: Vector[Double],
                        mus: Vector[Double],
                        sigma: Double)

  def mixture(p: Parameters): Generator[Double] = {
    val ks = p.thetas.indices
    val catParams = ks.zip(p.thetas.map(Real(_))).toMap
    for {
      i <- Categorical(catParams).generator
      sample <- Normal(p.mus(i), p.sigma).generator
    } yield sample
  }

  val p = Parameters(Vector(0.3, 0.2, 0.5), Vector(1, -2, 3), 0.5)
  val sims = mixture(p).repeat(10000).get

  val pw = new PrintWriter(new File("data/mixture_model.csv"))
  pw.write(sims.mkString("\n"))
  pw.close()
}

object MixtureModel extends App {
  implicit val rng = ScalaRNG(1)

  val sims: Vector[Double] = scala.io.Source
    .fromFile("data/mixture_model.csv")
    .getLines
    .map(_.toDouble)
    .toVector

  def normalise(alphas: Seq[Real]) = {
    val total = alphas.reduce(_ + _)
    alphas.map(a => a / total)
  }

  val model = for {
    unnormThetas <- RandomVariable.traverse(Vector.fill(3)(Gamma(3.0, 1.0).param))
    thetas = normalise(unnormThetas)
    mus <- RandomVariable.traverse(Vector.fill(3)(Normal(0, 1).param))
    sigma <- Exponential(3.0).param
    dists = mus.
      map(mu => Normal(mu, sigma))
    components: Map[Continuous, Real] = dists.zip(thetas).toMap
    _ <- Mixture(components).fit(sims)
  } yield Map("theta1" -> thetas.head,
        "theta2" -> thetas(1),
        "theta3" -> thetas(2),
        "mu1" -> mus.head,
        "mu2" -> mus(1),
        "mu3" -> mus(2),
        "sigma" -> sigma)

  // write two parallel chains
  implicit val contextShift = IO.contextShift(global)
  ParallelChains.run(model, 2, "data/mixture").unsafeRunSync()
}
