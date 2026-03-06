package Game

import scala.collection.mutable

case class SidePot(amount: Int, eligible: Set[String])

class Pot {
  private var _total: Int = 0
  private val contributions: mutable.Map[String, Int] = mutable.Map()

  def addBet(playerName: String, amount: Int): Unit = {
    _total += amount
    contributions(playerName) = contributions.getOrElse(playerName, 0) + amount
  }

  def total: Int = _total

  def sidePots(activePlayers: List[Player]): List[SidePot] = {
    val levels    = contributions.values.toList.distinct.sorted
    var remaining = contributions.toMap
    levels.flatMap { cap =>
      val slice    = remaining.collect { case (name, amt) if amt > 0 => name -> math.min(amt, cap) }
      remaining    = remaining.map { case (name, amt) => name -> math.max(0, amt - cap) }
      val eligible = slice.keys.filter(n => activePlayers.exists(_.name == n)).toSet
      if (slice.values.sum > 0) Some(SidePot(slice.values.sum, eligible)) else None
    }
  }

  def reset(): Unit = {
    _total = 0
    contributions.clear()
  }
}
