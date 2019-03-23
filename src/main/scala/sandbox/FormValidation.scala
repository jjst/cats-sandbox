package sandbox

import cats.data.Validated
import cats.syntax.either._
import cats.syntax.semigroupal._
import cats.syntax.apply._
import cats.instances.list._

object FormValidation extends App {

  type FormData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  case class User(name: String, age: Int)

  def getValue(fieldName: String)(formData: FormData): FailFast[String] =
    formData.get(fieldName).toRight(List(s"Field $fieldName not specified"))

  def parseInt(fieldName: String)(text: String): FailFast[Int] =
    Either
      .catchOnly[NumberFormatException](text.toInt)
      .leftMap(_ => List(s"$fieldName is not a valid number: $text"))

  def nonBlank(fieldName: String)(txt: String): FailFast[String] =
    txt
      .asRight[List[String]]
      .ensure(List(s"$fieldName cannot be blank"))(_.nonEmpty)

  def nonNegative(fieldName: String)(num: Int): FailFast[Int] =
    num
      .asRight[List[String]]
      .ensure(List(s"$fieldName cannot be negative"))(_ >= 0)

  def readName(formData: FormData): FailFast[String] = {
    for {
      value <- getValue("name")(formData)
      nonBlank <- nonBlank("name")(value)
    } yield nonBlank
  }

  def readAge(formData: FormData): FailFast[Int] = {
    for {
      textValue <- getValue("age")(formData)
      intValue <- parseInt("age")(textValue)
      nonNegative <- nonNegative("age")(intValue)
    } yield nonNegative
  }

  def readUser(formData: FormData): FailSlow[User] = {
    (
      readName(formData).toValidated,
      readAge(formData).toValidated
    ).mapN(User.apply)
  }

  println(readUser(Map("name" -> "mark", "age" -> "34")))

  println(readUser(Map("name" -> "", "age" -> "-4")))
}
