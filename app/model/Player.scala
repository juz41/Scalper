package model

/** Distinguishes human from AI-controlled players. */
sealed trait PlayerType
object PlayerType {
  case object Human    extends PlayerType
  case object Computer extends PlayerType
}

/**
 * A player's current set of cards.
 * Displayed with card indices so the human can refer to them during exchanges.
 */
case class Hand(cards: List[Card]) {
  def size: Int = cards.size
  override def toString: String =
  cards.map(_.toString).mkString("  ")
}
object Hand {
  val empty: Hand = Hand(List.empty)
}

/**
 * Represents a player at the table.
 * Why immutable: every action (bet, fold, receive cards) produces a new
 * Player value, which makes the game loop easy to follow and test.
 */
case class Player(
  name:       String,
  chips:      Int,
  hand:       Hand,
  playerType: PlayerType,
  folded:     Boolean = false
) {
  def withHand(h: Hand): Player  = copy(hand = h)
  def addChips(n: Int): Player   = copy(chips = chips + n)
  def removeChips(n: Int): Player = copy(chips = chips - n)
  def fold: Player               = copy(folded = true)
}
