package sandbox

trait Printable[A] {
  def format(a: A): String
}

object PrintableInstances {
  implicit val stringPrintable = new Printable[String] {
    override def format(s: String): String = s
  }

  implicit val intPrintable = new Printable[Int] {
    override def format(i: Int): String = i.toString
  }
}

object Printable {
  def format[A](a: A)(implicit printable: Printable[A]): String = printable.format(a)

  def print[A](a: A)(implicit printable: Printable[A]): Unit = Console.print(format(a))
}

object PrintableSyntax {
  implicit class PrintableOps[A](a: A) {
    def format(implicit printable: Printable[A]): String = Printable.format(a)

    def print(implicit printable: Printable[A]): Unit = Printable.print(a)
  }
}

object PrintableCat extends App {
  implicit val catPrintable = new Printable[Cat] {
    override def format(cat: Cat): String =
      s"${cat.name} is a ${cat.age} year old ${cat.color} cat."
  }

  val zoe = Cat(name = "Zoé", age = 15, color = "black")

  import PrintableSyntax._

  zoe.print
}

