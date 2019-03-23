package casestudies

import cats.{Applicative, Id}
import cats.syntax.traverse._
import cats.syntax.functor._
import cats.instances.list._ // for Traverse

import scala.concurrent.Future

object UptimeClient extends App {
  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  trait RealUptimeClient extends UptimeClient[Future] {
    override def getUptime(hostname: String): Future[Int]
  }

  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    def getUptime(hostname: String): Int = {
      hosts.getOrElse(hostname, 0)
    }
  }

  class UptimeService[F[_]](client: UptimeClient[F]) {
    def getTotalUptime(hostnames: List[String])(implicit a: Applicative[F]): F[Int] = {
      hostnames.traverse(client.getUptime).map(_.sum)
    }
  }

  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }
  testTotalUptime()
}
