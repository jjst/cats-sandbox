package casestudies

import casestudies.DataValidation.Predicate.Predicate
import cats.data.{NonEmptyList, Validated}
import cats.data.Validated._
import cats.kernel.Semigroup
import cats.syntax.semigroup._
import cats.syntax.either._
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.semigroupal._
import cats.syntax.apply._
import cats.syntax.validated._

object DataValidation extends App {

  object Predicate {
    type CheckResult[E, A] = Validated[E, A]

    trait Predicate[E, A] { self =>
      def apply(a: A)(implicit es: Semigroup[E]): CheckResult[E, A] = self match {
        case Pure(func) =>
          func(a)

        case And(left, right) =>
          (left(a), right(a)).mapN((_, _) => a)

        case Or(left, right) =>
          left(a) match {
            case Valid(_) => Valid(a)
            case Invalid(e1) => right(a) match {
              case Valid(_) => Valid(a)
              case Invalid(e2) => Invalid(e1 |+| e2)
            }
          }
      }

      def and(that: Predicate[E, A]): Predicate[E, A] =
        Predicate.And(self, that)


      def or(that: Predicate[E, A]): Predicate[E, A] =
        Predicate.Or(self, that)

    }
    def apply[E, A](f: A => CheckResult[E, A]): Predicate[E, A] = Predicate.Pure(f)
    def lift[E, A](e: E, f: A => Boolean): Predicate[E, A] = Predicate { a =>
      if (f(a)) Valid(a)
      else Invalid(e)
    }

    final case class Pure[E, A](fn: A => CheckResult[E, A]) extends Predicate[E, A]
    final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
    final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

  }

  object Check {
    sealed trait Check[E, A, B] {
      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

      def map[C](func: B => C): Check[E, A, C] =
        Map[E, A, B, C](this, func)

      def flatMap[C](func: B => Check[E, A, C]): Check[E, A, C] =
        FlatMap[E, A, B, C](this, func)

      def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
        AndThen[E, A, B, C](this, that)
    }

    case class Pure[E, A](predicate: Predicate[E, A]) extends Check[E, A, A] {
      override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = predicate(a)
    }

    case class Map[E, A, B, C](check: Check[E, A, B], fn: B => C) extends Check[E, A, C] {
      def apply(a: A)(implicit s: Semigroup[E]) = check(a).map(fn)
    }

    case class FlatMap[E, A, B, C](check: Check[E, A, B], fn: B => Check[E, A, C]) extends Check[E, A, C] {
      def apply(a: A)(implicit s: Semigroup[E]) =
        check(a).withEither { _.flatMap(fn(_)(a).toEither) }
    }

    case class AndThen[E, A, B, C](first: Check[E, A, B], second: Check[E, B, C]) extends Check[E, A, C] {
      def apply(a: A)(implicit s: Semigroup[E]) =
        first(a).withEither { _.flatMap(second(_).toEither) }
    }

    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] = Pure(pred)
  }



  type Errors = NonEmptyList[String]
  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)
  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.size > n)
  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit))
  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char"),
      str => str.contains(char))
  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.filter(c => c == char).size == 1)

  val validUserName = Check(longerThan(4) and alphanumeric)

  println(validUserName("test"))
  println(validUserName("test4"))
  println(validUserName("x34."))

}
