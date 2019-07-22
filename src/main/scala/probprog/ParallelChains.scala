package probprog

import com.stripe.rainier.compute._
import com.stripe.rainier.core._
import com.stripe.rainier.sampler._
import java.io._
import cats.implicits._
import cats.effect._
import cats.effect.IO._

object ParallelChains {
  def runChain(model: RandomVariable[Map[String, Real]])(implicit rng: RNG): IO[List[Map[String, Double]]] = IO {
    val thin = 5
    model.sample(HMC(5), 10000, 10000 * thin, thin)
  }

  def writeChain(chainNo: Int, chain: List[Map[String, Double]], filename: String): IO[Unit] = IO {
    val pw = new PrintWriter(new File(s"${filename}_$chainNo.csv"))
    pw.write(chain.head.keys.mkString(",") ++ "\n")
    pw.write(chain.map(_.values.mkString(",")).mkString("\n"))
    pw.close
  }

  def run(model: RandomVariable[Map[String, Real]],
          chains: Int, filename: String)(implicit rng: RNG, cs: ContextShift[IO]) = {
    List.range(1, chains + 1).
      parTraverse { i => for {
                     iters <- runChain(model)
                     io <- writeChain(i, iters, filename)
                   } yield io
      }
  }
}
