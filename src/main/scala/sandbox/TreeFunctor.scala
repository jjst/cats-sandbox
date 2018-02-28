package sandbox

import cats.Functor
import cats._
import cats.implicits._

object TreeFunctor extends App {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A])
    extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  object Tree {
    def leaf[A](value: A): Tree[A] = Leaf(value)

    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  }

  implicit val treeFunctor = new Functor[Tree] {
    override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value) => Leaf(f(value))
    }
  }

  val tree = Tree.leaf(1)
  println(tree.map(_ * 2))
  val tree2 = Tree.branch(Leaf(1), Branch(Leaf(2), Leaf(10)))
  println(tree2.map(_ * 2))
}
