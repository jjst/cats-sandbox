package sandbox

import cats.Monoid
import cats.syntax.semigroup._

object SuperAdder {
  def add[A: Monoid](items: List[A]): A = {
    items.foldLeft(Monoid.empty) { _ |+| _ }
  }

  case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoid = new Monoid[Order] {
    override def empty: Order = Order(0, 0)

    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }
}
