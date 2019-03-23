package sandbox

import cats.kernel.Monoid
import cats.syntax.monoid._

object Fold2 {

  def map[A, B](xs: List[A])(f: A => B): List[B] =
    xs.foldRight(List.empty[B]) { f(_) :: _ }

  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] =
    xs.foldRight(List.empty[B]) { f(_) ::: _ }

  def filter[A](xs: List[A])(f: A => Boolean): List[A] =
    xs.foldRight(List.empty[A]) { (a, xs) => if (f(a)) { a :: xs } else xs }

  def sum[A: Monoid](xs: List[A]): A =
    xs.foldRight(Monoid[A].empty)(_ |+| _)
}
