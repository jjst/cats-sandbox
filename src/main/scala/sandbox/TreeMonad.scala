package sandbox

import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._

object TreeMonad extends App {
  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A])
    extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      case Leaf(value) => f(value)
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      f(a) match {
        case Branch(left, right) => {
          Branch(
            flatMap(left) {
              case Left(a) => tailRecM(a)(f)
              case Right(b) => pure(b)
            },
            flatMap(right) {
              case Left(a) => tailRecM(a)(f)
              case Right(b) => pure(b)
            }
          )
        }
        case Leaf(Left(value)) => tailRecM(value)(f)
        case Leaf(Right(value)) => Leaf(value)
      }
    }

    override def pure[A](x: A): Tree[A] = leaf(x)
  }

  val t = for {
    a <- branch(leaf(100), leaf(200))
    b <- branch(leaf(a - 10), leaf(a + 10))
    c <- branch(leaf(b - 1), leaf(b + 1))
  } yield c

  println(t)
}
