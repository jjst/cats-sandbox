package casestudies

import cats.syntax.semigroup._
import cats.instances.map._
import sun.misc.GC

object CRDTs extends App {
  import cats.Monoid
  trait BoundedSemiLattice[A] extends Monoid[A] {
    def combine(a1: A, a2: A): A
    def empty: A
  }

  object BoundedSemiLattice {
    implicit val intBoundedSemiLattice: BoundedSemiLattice[Int] = new BoundedSemiLattice[Int] {
      override def combine(a1: Int, a2: Int): Int = a1 max a2

      override def empty: Int = 0
    }

    implicit def setBoundedSemiLattice[A]: BoundedSemiLattice[Set[A]] = new BoundedSemiLattice[Set[A]] {
      override def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 union a2

      override def empty: Set[A] = Set.empty
    }
  }

  trait GCounter[F[_,_],K, V] {
    def increment(f: F[K, V])(k: K, v: V)
                 (implicit m: Monoid[V]): F[K, V]
    def merge(f1: F[K, V], f2: F[K, V])
             (implicit b: BoundedSemiLattice[V]): F[K, V]
    def total(f: F[K, V])
             (implicit m: Monoid[V]): V
  }
  object GCounter {
    def apply[F[_,_], K, V]
    (implicit counter: GCounter[F, K, V]) =
      counter
  }

  implicit def mapInstance[K, V] = new GCounter[Map, K, V] {
    override def increment(map: Map[K, V])(k: K, v: V)(implicit m: Monoid[V]): Map[K, V] = {
      val newValue = map.getOrElse(k, Monoid.empty[V]) |+| v
      map + (k -> newValue)
    }

    override def merge(m1: Map[K, V], m2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] =
      m1 |+| m2

    override def total(map: Map[K, V])(implicit m: Monoid[V]): V =
      m.combineAll(map.values)
  }

  import cats.instances.int._ // for Monoid

  val g1 = Map("a" -> 7, "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)

  val counter = GCounter[Map, String, Int]
  val merged = counter.merge(g1, g2)
  // merged: Map[String,Int] = Map(a -> 7, b -> 5)
  val total = counter.total(merged)

  println(total)
}
