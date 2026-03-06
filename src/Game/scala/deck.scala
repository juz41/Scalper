package Game

import scala.collection.mutable
import scala.util.Random

class Deck(val config: DeckConfig) {

  private def freshShuffled: List[Card] = Random.shuffle(
    for {
      s <- config.suits
      r <- config.ranks
    } yield Card(r, s)
  )

  private val cards: mutable.Queue[Card] = mutable.Queue(freshShuffled*)

  def deal(): Card = {
    require(cards.nonEmpty, "Deck is empty!")
    cards.dequeue()
  }

  def remaining: Int = cards.size

  def reset(): Unit = {
    cards.clear()
    cards.enqueueAll(freshShuffled)
  }
}
