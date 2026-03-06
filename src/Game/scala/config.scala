package Game

trait HandEvaluatorStrategy {
  /** Number of hole cards dealt to each player. */
  def holeCardCount: Int

  /** Derive the best hand for a player given their hole cards and the board. */
  def bestHand(holeCards: List[Card], communityCards: List[Card], deckConfig: DeckConfig): EvaluatedHand
}

object TexasHoldemStrategy extends HandEvaluatorStrategy {
  val holeCardCount: Int = 2

  def bestHand(holeCards: List[Card], communityCards: List[Card], deckConfig: DeckConfig): EvaluatedHand = {
    require(holeCards.length == holeCardCount,
      s"Texas Hold'em needs $holeCardCount hole cards, got ${holeCards.length}")
    require(communityCards.length >= 3,
      s"Need at least 3 community cards, got ${communityCards.length}")

    (holeCards ++ communityCards)
      .combinations(5)
      .map(CoreEvaluator.evaluate5(_, deckConfig))
      .max
  }
}

object OmahaStrategy extends HandEvaluatorStrategy {
  val holeCardCount: Int = 4

  def bestHand(holeCards: List[Card], communityCards: List[Card], deckConfig: DeckConfig): EvaluatedHand = {
    require(holeCards.length == holeCardCount,
      s"Omaha needs $holeCardCount hole cards, got ${holeCards.length}")
    require(communityCards.length >= 3,
      s"Need at least 3 community cards, got ${communityCards.length}")

    (for {
      hCombo <- holeCards.combinations(2)
      cCombo <- communityCards.combinations(3)
    } yield CoreEvaluator.evaluate5(hCombo ++ cCombo, deckConfig)).max
  }
}

// ─── Deck Config ───────────────────────────────────────────────────────────────

/**
 * Describes the shape of the deck.
 * @param numRanks  How many distinct rank values (e.g. 13 for standard)
 * @param numSuits  How many distinct suits       (e.g.  4 for standard)
 * @param highRank  Rank treated as "Ace high" for royal-flush detection. Defaults to numRanks.
 * @param lowRank   Rank treated as "Ace low" for wheel straights. Defaults to 1.
 */
case class DeckConfig(
  numRanks: Int,
  numSuits: Int,
  highRank: Int = -1,
  lowRank:  Int = 1
) {
  val resolvedHighRank: Int    = if (highRank == -1) numRanks else highRank
  val ranks:            List[Int] = (1 to numRanks).toList
  val suits:            List[Int] = (1 to numSuits).toList

  override def toString: String = s"${numRanks}R×${numSuits}S"
}

object DeckConfig {
  val Standard: DeckConfig = DeckConfig(numRanks = 13, numSuits = 4, highRank = 13, lowRank = 1)
}

// ─── Game Config ───────────────────────────────────────────────────────────────

/**
 * Single source of truth for all game rules.
 *
 * @param name        Display name for the game variant
 * @param deckConfig  Shape of the deck (ranks, suits, high/low rank)
 * @param evaluator   Strategy that defines hole card count + hand evaluation logic
 * @param smallBlind  Small blind amount
 * @param bigBlind    Big blind amount
 */
case class GameConfig(
  name:       String,
  deckConfig: DeckConfig,
  evaluator:  HandEvaluatorStrategy,
  smallBlind: Int,
  bigBlind:   Int
) {
  override def toString: String = s"$name [$deckConfig]"
}

object GameConfig {
  /** Standard Texas Hold'em — 52-card deck. */
  val TexasHoldem: GameConfig = GameConfig(
    name       = "Texas Hold'em",
    deckConfig = DeckConfig.Standard,
    evaluator  = TexasHoldemStrategy,
    smallBlind = 10,
    bigBlind   = 20
  )

  /** Standard Omaha — 52-card deck. */
  val Omaha: GameConfig = GameConfig(
    name       = "Omaha",
    deckConfig = DeckConfig.Standard,
    evaluator  = OmahaStrategy,
    smallBlind = 10,
    bigBlind   = 20
  )
}
