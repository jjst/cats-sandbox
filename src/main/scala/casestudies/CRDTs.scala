package casestudies

object CRDTs {
  final case class GCounter(counters: Map[String, Int]) {
    def increment(machine: String, amount: Int): GCounter = {
      val value = amount + counters.getOrElse(machine, 0)
      GCounter(counters + (machine -> value))
    }

    def merge(that: GCounter): GCounter = GCounter {
      (this.counters.keySet ++ that.counters.keySet).map { key =>
        (key, List(this.counters.get(key), that.counters.get(key)).flatten.max)
      } toMap
    }

    def total: Int =
      counters.values.sum
  }
}
