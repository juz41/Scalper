package model

/** The four suits of a standard deck. */
sealed trait Suit {
  def symbol: String
}
object Suit {
  case object Hearts   extends Suit { val symbol = "H" }
  case object Diamonds extends Suit { val symbol = "D" }
  case object Clubs    extends Suit { val symbol = "C" }
  case object Spades   extends Suit { val symbol = "S" }

  val all: List[Suit] = List(Hearts, Diamonds, Clubs, Spades)
}

/**
 * Numeric rank of a card.
 * Why a case class instead of an enum: lets us keep the numeric `value`
 * alongside the display `name` while remaining easy to compare.
 */
case class Rank(value: Int, name: String)
object Rank {
  val Two   = Rank(2,  "2")
  val Three = Rank(3,  "3")
  val Four  = Rank(4,  "4")
  val Five  = Rank(5,  "5")
  val Six   = Rank(6,  "6")
  val Seven = Rank(7,  "7")
  val Eight = Rank(8,  "8")
  val Nine  = Rank(9,  "9")
  val Ten   = Rank(10, "T")
  val Jack  = Rank(11, "J")
  val Queen = Rank(12, "Q")
  val King  = Rank(13, "K")
  val Ace   = Rank(14, "A")

  val all: List[Rank] =
    List(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)
}

/** A single playing card. */
case class Card(rank: Rank, suit: Suit) {
  override def toString: String = s"${rank.name}${suit.symbol}"
}
