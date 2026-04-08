package logic

import model.Hand
import model.Card

/**
 * A fully evaluated hand that carries tiebreaker values for showdowns.
 * `tiebreakers` is ordered so the most significant discriminator comes first.
 */
case class EvaluatedHand(rank: HandRank, tiebreakers: List[Int])
    extends Ordered[EvaluatedHand] {
  override def compare(that: EvaluatedHand): Int = {
    val rankCmp = this.rank.compare(that.rank)
    if (rankCmp != 0) rankCmp
    else
      this.tiebreakers
        .zip(that.tiebreakers)
        .collectFirst { case (a, b) if a != b => a.compareTo(b) }
        .getOrElse(0)
  }
}

/**
 * Stateless evaluator for poker hands.
 *
 * Supports:
 *  - evaluate(hand)           – evaluates exactly 5 cards
 *  - bestFiveFrom(cards)      – picks the best 5-card hand from any N >= 5 cards
 *                               (used for Texas Hold'em: 2 hole + 5 community = 7)
 *  - determineWinner(hands)   – resolves a showdown across multiple hands
 */
object HandEvaluator {

  /** Evaluates a 5-card hand. Requires exactly 5 cards. */
  def evaluate(hand: Hand): EvaluatedHand = {
    require(hand.size == 5, s"evaluate() requires exactly 5 cards, got ${hand.size}")
    eval5(hand.cards)
  }

  /**
   * Picks the best possible 5-card hand from `cards` (N >= 5).
   * Why: Texas Hold'em players choose the best 5 from their 2 hole cards
   * plus 5 community cards (7 total). C(7,5) = 21 combinations to check.
   */
  def bestFiveFrom(cards: List[Card]): EvaluatedHand = {
    require(cards.size >= 5, s"Need at least 5 cards, got ${cards.size}")
    cards.combinations(5).map(c => eval5(c)).max
  }

  /**
   * Returns the winner(s) from a showdown.
   * Each entry is (playerName, holeCards, communityCards).
   * Multiple entries are returned only on an exact tie.
   */
  def determineWinner(
    entries: List[(String, List[Card], List[Card])]
  ): List[(String, EvaluatedHand)] = {
    val evaluated = entries.map { case (name, hole, community) =>
      (name, bestFiveFrom(hole ++ community))
    }
    val best = evaluated.map(_._2).max
    evaluated.filter(_._2 == best)
  }

  // Private core evaluator (exactly 5 cards)

  private def eval5(cards: List[Card]): EvaluatedHand = {
    val values     = cards.map(_.rank.value)
    val sortedDesc = values.sorted.reverse
    val isFlush    = cards.map(_.suit).distinct.size == 1
    val isStraight = checkStraight(sortedDesc)
    val groupSizes = values.groupBy(identity).values.map(_.size).toList.sorted.reverse

    if (isFlush && isStraight) {
      if (sortedDesc.head == 14) EvaluatedHand(HandRank.RoyalFlush, sortedDesc)
      else                       EvaluatedHand(HandRank.StraightFlush, sortedDesc)
    } else if (groupSizes == List(4, 1)) {
      EvaluatedHand(HandRank.FourOfAKind, groupedTiebreakers(values))
    } else if (groupSizes == List(3, 2)) {
      EvaluatedHand(HandRank.FullHouse, groupedTiebreakers(values))
    } else if (isFlush) {
      EvaluatedHand(HandRank.Flush, sortedDesc)
    } else if (isStraight) {
      EvaluatedHand(HandRank.Straight, sortedDesc)
    } else if (groupSizes == List(3, 1, 1)) {
      EvaluatedHand(HandRank.ThreeOfAKind, groupedTiebreakers(values))
    } else if (groupSizes == List(2, 2, 1)) {
      EvaluatedHand(HandRank.TwoPair, groupedTiebreakers(values))
    } else if (groupSizes == List(2, 1, 1, 1)) {
      EvaluatedHand(HandRank.OnePair, groupedTiebreakers(values))
    } else {
      EvaluatedHand(HandRank.HighCard, sortedDesc)
    }
  }

  private def checkStraight(sortedDesc: List[Int]): Boolean = {
    val normal = sortedDesc.zip(sortedDesc.tail).forall { case (a, b) => a - b == 1 }
    val wheel  = sortedDesc == List(14, 5, 4, 3, 2)
    normal || wheel
  }

  private def groupedTiebreakers(values: List[Int]): List[Int] =
    values
      .groupBy(identity)
      .toList
      .sortBy { case (v, group) => (-group.size, -v) }
      .flatMap { case (v, group) => List.fill(group.size)(v) }
}
