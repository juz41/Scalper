package engine

import logic.{HandEvaluator, HandRank}
import model.{Card, Deck, Hand, Rank, Suit}
import scala.util.Random

/**
 * Hand-strength-aware poker AI.
 *
 * Uses the existing [[HandEvaluator]] to actually evaluate hand quality
 * and makes decisions based on:
 *   - Pre-flop hand ranking (pair quality, suited connectors, high cards)
 *   - Post-flop Monte Carlo win-rate estimation (sampling opponent hands)
 *   - Pot odds comparison against estimated equity
 *   - Position-aware range widening (late position plays wider)
 *   - Bluff frequency scaled by street and board texture
 *
 * Raise semantics: Raise(total) means "my total contribution this street
 * should be `total` chips", matching what [[GameEngine]] expects.
 */
object ComputerAI {

  private val rng = new Random()

  /** Number of random opponent hands to sample for post-flop equity. */
  private val MonteCarloSamples = 200

  // ── Public entry point ──────────────────────────────────────────────────────

  /**
   * Choose an action given the complete observable game state.
   *
   * @param ctx A [[GameContext]] snapshot built just before this player must act.
   * @return    An [[Action]]; [[Action.Raise]](total) expresses a total street bet.
   */
  def decideAction(ctx: GameContext): Action = {
    val strength = estimateStrength(ctx)

    // Never fold when checking is free
    if (ctx.toCall == 0) {
      return decideWhenFreeCheck(ctx, strength)
    }

    val equity     = strength
    val potOdds    = ctx.potOdds                         // fraction of pot we pay
    val callFrac   = if (ctx.availableChips > 0)
                       ctx.toCall.toDouble / ctx.availableChips
                     else 1.0

    // ── Strong hand / clear equity advantage ──────────────────────────────
    if (equity > 0.75) {
      // Monster — raise for value
      return buildValueRaise(ctx, equity)
    }

    if (equity > 0.55) {
      // Good hand — raise sometimes, call otherwise
      if (rng.nextDouble() < 0.45 && ctx.availableChips > ctx.toCall)
        return buildValueRaise(ctx, equity)
      else
        return Action.Call
    }

    // ── Marginal hand — compare equity to pot odds ────────────────────────
    if (equity > potOdds + 0.05) {
      // We have enough equity to call profitably
      // Occasionally raise as a semi-bluff (especially on flop/turn with draws)
      if (equity > 0.40 && rng.nextDouble() < 0.20 && ctx.availableChips > ctx.toCall)
        return buildSemiBluffRaise(ctx)
      else
        return Action.Call
    }

    // ── Bluff opportunity on river ────────────────────────────────────────
    if (ctx.street == Street.River && equity < 0.30 && rng.nextDouble() < 0.12) {
      if (ctx.availableChips > ctx.toCall)
        return buildSemiBluffRaise(ctx)
    }

    // ── Weak hand facing aggression — fold threshold ──────────────────────
    if (ctx.facingAggression && ctx.raisesThisStreet >= 2 && equity < 0.35) {
      return Action.Fold
    }

    // Stack pressure: committing too many chips with a marginal hand
    if (callFrac > 0.5 && equity < 0.40) return Action.Fold
    if (callFrac > 0.3 && equity < 0.25) return Action.Fold

    // Default: if equity is somewhat close to pot odds, coin-flip call/fold
    if (equity > potOdds - 0.05)
      Action.Call
    else
      Action.Fold
  }

  // ── When checking is free ───────────────────────────────────────────────────

  private def decideWhenFreeCheck(ctx: GameContext, strength: Double): Action = {
    // Strong hand → bet for value
    if (strength > 0.70 && ctx.availableChips > 0) {
      return buildValueRaise(ctx, strength)
    }
    // Medium hand → bet sometimes for protection/thin value
    if (strength > 0.45 && rng.nextDouble() < 0.35 && ctx.availableChips > 0) {
      return buildValueRaise(ctx, strength)
    }
    // Weak hand → occasional bluff
    if (strength < 0.25 && rng.nextDouble() < 0.15 && ctx.availableChips > 0) {
      return buildSemiBluffRaise(ctx)
    }
    // Otherwise just check
    Action.Call
  }

  // ── Hand strength estimation ────────────────────────────────────────────────

  private def estimateStrength(ctx: GameContext): Double = {
    ctx.street match {
      case Street.PreFlop => preFlopStrength(ctx)
      case _              => postFlopEquity(ctx)
    }
  }

  /**
   * Pre-flop hand ranking: scores hole cards on a 0.0–1.0 scale.
   * Based on standard starting hand strength charts.
   * Position bonus: late positions (BTN, CO) get a small range-widening boost.
   */
  private def preFlopStrength(ctx: GameContext): Double = {
    val cards = ctx.holeCards.cards
    if (cards.size < 2) return 0.3 // safety

    val c1 = cards.head
    val c2 = cards(1)
    val high = c1.rank.value.max(c2.rank.value)
    val low  = c1.rank.value.min(c2.rank.value)
    val suited = c1.suit == c2.suit
    val isPair = c1.rank.value == c2.rank.value
    val gap = high - low

    var score: Double = 0.0

    if (isPair) {
      // Pairs: AA=1.0 → 22=0.45
      score = 0.45 + (high - 2) * (0.55 / 12.0)
    } else {
      // Base: average of card strengths (2=0.0 .. 14=1.0) scaled down
      val avgNorm = ((high - 2) + (low - 2)) / 24.0
      score = avgNorm * 0.55

      // Suited bonus
      if (suited) score += 0.08

      // Connectedness bonus (closer = better draws)
      if (gap == 1) score += 0.06
      else if (gap == 2) score += 0.03

      // High card bonus (both broadway)
      if (high >= 10 && low >= 10) score += 0.10
      else if (high >= 12) score += 0.04

      // Penalty for large gaps
      if (gap >= 5) score -= 0.05
    }

    // Position bonus: late position widens playable range
    val posBonus = ctx.position match {
      case "BTN" | "CO" => 0.06
      case "SB"         => -0.03    // out of position post-flop
      case _            => 0.0
    }

    (score + posBonus).max(0.05).min(0.99)
  }

  /**
   * Post-flop equity via Monte Carlo simulation.
   *
   * Deals random opponent hole cards from the remaining deck and
   * evaluates who wins using [[HandEvaluator.bestFiveFrom]].
   * Returns win-rate as 0.0–1.0.
   */
  private def postFlopEquity(ctx: GameContext): Double = {
    val myCards   = ctx.holeCards.cards
    val community = ctx.community
    if (community.isEmpty) return preFlopStrength(ctx) // safety fallback

    val knownCards = (myCards ++ community).toSet
    val remainingDeck = Deck.standard.cards.filterNot(knownCards.contains)

    if (remainingDeck.size < 2) return 0.5 // not enough cards to simulate

    val myBest = if (myCards.size + community.size >= 5) {
      Some(HandEvaluator.bestFiveFrom(myCards ++ community))
    } else None

    // For incomplete boards (e.g., flop with 3 community), we also need to
    // simulate the remaining community cards
    val communityNeeded = 5 - community.size
    val numOpponents = (ctx.numActivePlayers - 1).max(1)

    var wins   = 0
    var ties   = 0
    var total  = 0

    var i = 0
    while (i < MonteCarloSamples) {
      val shuffled = rng.shuffle(remainingDeck)

      // Deal remaining community cards (if any)
      val extraCommunity = shuffled.take(communityNeeded)
      val fullCommunity  = community ++ extraCommunity
      val afterCommunity = shuffled.drop(communityNeeded)

      // Deal opponent hands
      if (afterCommunity.size >= numOpponents * 2) {
        val myEval = HandEvaluator.bestFiveFrom(myCards ++ fullCommunity)

        var iWin  = true
        var iTie  = true
        var oIdx  = 0
        while (oIdx < numOpponents && (iWin || iTie)) {
          val oppCards = afterCommunity.slice(oIdx * 2, oIdx * 2 + 2)
          val oppEval  = HandEvaluator.bestFiveFrom(oppCards ++ fullCommunity)
          val cmp = myEval.compare(oppEval)
          if (cmp < 0) { iWin = false; iTie = false }
          else if (cmp > 0) { iTie = false }
          oIdx += 1
        }

        if (iWin && !iTie) wins += 1
        else if (iTie)     ties += 1
        total += 1
      }

      i += 1
    }

    if (total == 0) return 0.5
    (wins.toDouble + ties.toDouble * 0.5) / total.toDouble
  }

  // ── Raise builders ──────────────────────────────────────────────────────────

  /**
   * Build a value raise: stronger hands bet larger fractions of the pot.
   * strength 0.55 → ~40% pot; strength 0.90+ → ~100% pot (or overbet).
   */
  private def buildValueRaise(ctx: GameContext, strength: Double): Action = {
    val betSoFar = ctx.currentBet - ctx.toCall  // what we've already put in
    val potFraction = 0.3 + (strength - 0.5) * 1.4  // 0.3 at 0.5 → 1.0 at ~0.85

    val rawBet = (ctx.pot * potFraction.max(0.3).min(1.5)).toInt
    val desired = (ctx.toCall + rawBet).max(ctx.toCall + 1)

    // Add small jitter so we're not perfectly predictable
    val jitter = rng.nextInt(((rawBet * 0.1).toInt + 1).max(1))
    val total  = (desired + jitter).min(ctx.availableChips)

    if (total <= ctx.toCall) Action.Call else Action.Raise(total)
  }

  /**
   * Build a semi-bluff raise: smaller sizing (30–50% pot) to keep risk low.
   */
  private def buildSemiBluffRaise(ctx: GameContext): Action = {
    val potFraction = 0.3 + rng.nextDouble() * 0.2    // 30%–50% pot
    val rawBet  = (ctx.pot * potFraction).toInt.max(1)
    val desired = (ctx.toCall + rawBet).max(ctx.toCall + 1)
    val total   = desired.min(ctx.availableChips)

    if (total <= ctx.toCall) Action.Call else Action.Raise(total)
  }
}
