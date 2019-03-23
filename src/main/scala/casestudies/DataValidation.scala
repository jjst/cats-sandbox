package casestudies

import cats.data.Validated
import cats.kernel.Semigroup
import cats.syntax.semigroup._
import cats.syntax.either._
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.semigroupal._
import cats.syntax.apply._
import cats.syntax.validated._

object DataValidation extends App {

  object Check {
    type CheckResult[E, A] = Validated[E, A]

    trait Check[E, A] { self =>
      def apply(a: A)(implicit es: Semigroup[E]): CheckResult[E, A] = self match {
        case Pure(func) =>
          func(a)

        case And(left, right) =>
          (left(a), right(a)).mapN((_, _) => a)
      }

      def and(that: Check[E, A]): Check[E, A] = {
        Check.And(self, that)
      }

    }
    def apply[E, A](f: A => CheckResult[E, A]): Check[E, A] = Check.Pure(f)

    final case class Pure[E, A](fn: A => CheckResult[E, A]) extends Check[E, A]
    final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

  }

  val numberIsPositive =
    Check { num: Int =>
      num
        .valid[List[String]]
        .ensure(List("Number should be positive"))(_ >= 0)
    }

  val numberIsEven =
    Check { num: Int =>
      num
        .valid[List[String]]
        .ensure(List("Number should be even"))(_ % 2 == 0)
    }

  val positiveAndEvenCheck =
    numberIsPositive and numberIsEven

  println(positiveAndEvenCheck(3))

  println(positiveAndEvenCheck(-3))

  println(positiveAndEvenCheck(2))
}
