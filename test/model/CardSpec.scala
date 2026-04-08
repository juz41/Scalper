package model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CardSpec extends AnyWordSpec with Matchers {

  "Suit" should {
    "contain exactly 4 suits" in {
      Suit.all should have size 4
    }

    "have distinct symbols" in {
      Suit.all.map(_.symbol).distinct should have size 4
    }
  }

  "Rank" should {
    "contain exactly 13 ranks" in {
      Rank.all should have size 13
    }

    "have Two as the lowest rank" in {
      Rank.Two.value shouldBe 2
    }

    "have Ace as the highest rank" in {
      Rank.Ace.value shouldBe 14
    }

    "have strictly increasing values" in {
      Rank.all.map(_.value) shouldBe sorted
    }
  }

  "Card" should {
    "produce a compact string representation" in {
      Card(Rank.Ace,   Suit.Spades).toString  shouldBe "AS"
      Card(Rank.Ten,   Suit.Hearts).toString  shouldBe "TH"
      Card(Rank.Queen, Suit.Clubs).toString   shouldBe "QC"
    }

    "support value equality" in {
      Card(Rank.King, Suit.Hearts) shouldBe Card(Rank.King, Suit.Hearts)
    }

    "not equal a card with a different rank" in {
      Card(Rank.King,  Suit.Hearts) should not be Card(Rank.Queen, Suit.Hearts)
    }

    "not equal a card with a different suit" in {
      Card(Rank.King, Suit.Hearts) should not be Card(Rank.King, Suit.Spades)
    }
  }
}
