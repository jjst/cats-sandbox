package sandbox

object ProductOfMonads {
  import cats.Monad
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = {
    x.flatMap { x1 =>
      y.map { y2 =>
        (x1, y2)
      }
    }
  }
}
