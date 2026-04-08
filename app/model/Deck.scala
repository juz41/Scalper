package model

import scala.util.Random

/**
 * An immutable, ordered collection of cards.
 * All operations return a new Deck rather than mutating state –
 * this makes the game logic easy to test and reason about.
 */
case class Deck(cards: List[Card]) {

  /** Returns a new shuffled deck (does not modify this one). */
  def shuffle(rng: Random = Random): Deck = Deck(rng.shuffle(cards))

  /**
   * Deals `n` cards from the top.
   * Returns (dealt cards, remaining deck).
   * Why: keeps dealing pure – callers receive both results instead of
   * relying on shared mutable state.
   */
  def deal(n: Int): (List[Card], Deck) = {
    require(cards.size >= n, s"Cannot deal $n cards from a deck of ${cards.size}")
    (cards.take(n), Deck(cards.drop(n)))
  }

  def size: Int = cards.size
}

object Deck {
  /** Standard 52-card deck, unshuffled. */
  def standard: Deck = Deck(
    for {
      suit <- Suit.all
      rank <- Rank.all
    } yield Card(rank, suit)
  )
}
