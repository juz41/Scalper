package Game

/**
 * A card identified by numeric rank and suit.
 * @param rank  1-based rank value (1 = lowest, numRanks = highest)
 * @param suit  1-based suit number (1..numSuits)
 */
case class Card(rank: Int, suit: Int) {
  override def toString: String = s"R${rank}S${suit}"
}
