package model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DeckSpec extends AnyWordSpec with Matchers {

  "Deck.standard" should {
    "contain exactly 52 cards" in {
      Deck.standard.size shouldBe 52
    }

    "contain 52 unique cards" in {
      Deck.standard.cards.distinct should have size 52
    }

    "contain every rank–suit combination" in {
      val expected = for { s <- Suit.all; r <- Rank.all } yield Card(r, s)
      Deck.standard.cards.toSet shouldBe expected.toSet
    }
  }

  "Deck.shuffle" should {
    "keep the same number of cards" in {
      Deck.standard.shuffle().size shouldBe 52
    }

    "keep the same set of cards (just reordered)" in {
      Deck.standard.shuffle().cards.toSet shouldBe Deck.standard.cards.toSet
    }
  }

  "Deck.deal" should {
    "return the requested number of cards" in {
      val (cards, _) = Deck.standard.deal(5)
      cards should have size 5
    }

    "remove the dealt cards from the remaining deck" in {
      val (_, remaining) = Deck.standard.deal(5)
      remaining.size shouldBe 47
    }

    "not include dealt cards in the remaining deck" in {
      val (dealt, remaining) = Deck.standard.deal(5)
      remaining.cards should contain noElementsOf dealt
    }

    "deal cards in order (top of deck first)" in {
      val top5           = Deck.standard.cards.take(5)
      val (dealt, _)     = Deck.standard.deal(5)
      dealt shouldBe top5
    }

    "throw IllegalArgumentException when dealing more than available" in {
      an[IllegalArgumentException] should be thrownBy Deck.standard.deal(53)
    }

    "allow dealing the entire deck" in {
      val (all, rest) = Deck.standard.deal(52)
      all should have size 52
      rest.size shouldBe 0
    }
  }
}
