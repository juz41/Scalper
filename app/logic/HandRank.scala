package logic

/**
 * Sealed hierarchy of all standard poker hand rankings.
 * `value` is used for comparison – higher is stronger.
 */
sealed abstract class HandRank(val value: Int, val name: String)
    extends Ordered[HandRank] {
  override def compare(that: HandRank): Int = this.value.compareTo(that.value)
}

object HandRank {
  case object HighCard      extends HandRank(1,  "High Card")
  case object OnePair       extends HandRank(2,  "One Pair")
  case object TwoPair       extends HandRank(3,  "Two Pair")
  case object ThreeOfAKind  extends HandRank(4,  "Three of a Kind")
  case object Straight      extends HandRank(5,  "Straight")
  case object Flush         extends HandRank(6,  "Flush")
  case object FullHouse     extends HandRank(7,  "Full House")
  case object FourOfAKind   extends HandRank(8,  "Four of a Kind")
  case object StraightFlush extends HandRank(9,  "Straight Flush")
  case object RoyalFlush    extends HandRank(10, "Royal Flush")
}
