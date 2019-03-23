package sandbox

object Fold extends App {
  println(List(1, 2, 3).foldLeft(List.empty[Int])((xs, a) => a :: xs))

  println(List(1, 2, 3).foldRight(List.empty[Int])(_ :: _))
}
