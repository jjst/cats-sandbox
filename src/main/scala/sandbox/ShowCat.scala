package sandbox

import cats._
import cats.implicits._

object ShowCat extends App {
  implicit val catShow: Show[Cat] =
    Show.show(cat => s"${cat.name} is a ${cat.age} year old ${cat.color} cat.")


  val zoe = Cat(name = "Zoé", age = 15, color = "black")

  println(zoe.show)
}

