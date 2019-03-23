package sandbox

import cats.data.EitherT
import cats.data.EitherT
import cats.syntax.applicative._
import cats.instances.future._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.syntax.functor._

import scala.concurrent.{Await, Future}

object MonadTransformers {

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] = {
    EitherT(Future.successful(powerLevels.get(autobot).toRight(s"$autobot unreachable")))
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    for {
      powerLevel1 <- getPowerLevel(ally1)
      powerLevel2 <- getPowerLevel(ally2)
    } yield powerLevel1 + powerLevel2 > 15
  }

  def tacticalReport(ally1: String, ally2: String): String = {
    val report = Await.result(canSpecialMove(ally1, ally2).value, 10.seconds)
    report match {
      case Left(error) => s"Comms error: $error"
      case Right(false) => s"$ally1 and $ally2 need a recharge"
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
    }
  }
}
