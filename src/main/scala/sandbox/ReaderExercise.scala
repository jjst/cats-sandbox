package sandbox

import cats.data.Reader
import cats.syntax.applicative._


object ReaderExercise extends App {
  case class Db(
                 usernames: Map[Int, String],
                 passwords: Map[String, String]
               )

  type DbReader[T] = Reader[Db, T]

  def findUsername(userId: Int): DbReader[Option[String]] = {
    Reader(db => db.usernames.get(userId))
  }

  def checkPassword(username: String, password: String): DbReader[Boolean] = {
    Reader(db => db.passwords.get(username).contains(password))
  }

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
    for {
      userName <- findUsername(userId)
      passwordOk <- userName.fold(false.pure[DbReader]) {
        checkPassword(_, password)
      }
    } yield passwordOk
  }

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
    )
  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  println(checkLogin(1, "zerocool").run(db))
  println(checkLogin(4, "davinci").run(db))
}
