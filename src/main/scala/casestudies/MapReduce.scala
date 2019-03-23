package casestudies

import cats.Monoid
import cats.instances.string._
import cats.instances.vector._
import cats.instances.future._
import cats.instances.int._
import cats.syntax.traverse._
import cats.syntax.foldable._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object MapReduce extends App {
  def foldMap[A, B: Monoid](xs: Vector[A])(fn: A => B): B = {
    xs.map(fn).fold(Monoid[B].empty)(Monoid[B].combine)
  }

  import scala.concurrent.ExecutionContext.Implicits.global

  def parallelFoldMap[A, B : Monoid]
    (values: Vector[A])
    (func: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt

    val batches = values.grouped(groupSize)

    val futures = batches.map(values => Future(foldMap(values)(func))).toVector

    futures.sequence.map(_.fold(Monoid[B].empty)(Monoid[B].combine))
  }

  def parallelFoldMap2[A, B : Monoid]
  (values: Vector[A])
  (func: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt

    values
      .grouped(groupSize)
      .toVector
      .traverse(group => Future(group.foldMap(func)))
      .map(_.combineAll)
  }

  val result: Future[Int] =
    parallelFoldMap((1 to 1000000).toVector)(identity)
  println(Await.result(result, 1.second))
}
