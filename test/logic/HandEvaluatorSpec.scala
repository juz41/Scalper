package logic

import model._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HandEvaluatorSpec extends AnyWordSpec with Matchers {

  private def card(r: Rank, s: Suit): Card = Card(r, s)
  private def makeHand(cards: (Rank, Suit)*): Hand =
    Hand(cards.map { case (r, s) => Card(r, s) }.toList)

  // evaluate() – 5-card hand detection

  "HandEvaluator.evaluate" when {

    "given a Royal Flush" should {
      "return RoyalFlush" in {
        val h = makeHand(
          (Rank.Ace, Suit.Spades), (Rank.King, Suit.Spades),
          (Rank.Queen, Suit.Spades), (Rank.Jack, Suit.Spades), (Rank.Ten, Suit.Spades)
        )
        HandEvaluator.evaluate(h).rank shouldBe HandRank.RoyalFlush
      }
    }

    "given a Straight Flush" should {
      "return StraightFlush" in {
        val h = makeHand(
          (Rank.Nine, Suit.Hearts), (Rank.Eight, Suit.Hearts),
          (Rank.Seven, Suit.Hearts), (Rank.Six, Suit.Hearts), (Rank.Five, Suit.Hearts)
        )
        HandEvaluator.evaluate(h).rank shouldBe HandRank.StraightFlush
      }
    }

    "given Four of a Kind" should {
      "return FourOfAKind" in {
        val h = makeHand(
          (Rank.Ace, Suit.Hearts), (Rank.Ace, Suit.Diamonds),
          (Rank.Ace, Suit.Clubs), (Rank.Ace, Suit.Spades), (Rank.King, Suit.Hearts)
        )
        HandEvaluator.evaluate(h).rank shouldBe HandRank.FourOfAKind
      }
    }

    "given a Full House" should {
      "return FullHouse" in {
        val h = makeHand(
          (Rank.King, Suit.Hearts), (Rank.King, Suit.Diamonds), (Rank.King, Suit.Clubs),
          (Rank.Two, Suit.Hearts), (Rank.Two, Suit.Diamonds)
        )
        HandEvaluator.evaluate(h).rank shouldBe HandRank.FullHouse
      }
    }

    "given a Flush" should {
      "return Flush" in {
        val h = makeHand(
          (Rank.Ace, Suit.Clubs), (Rank.Jack, Suit.Clubs),
          (Rank.Nine, Suit.Clubs), (Rank.Seven, Suit.Clubs), (Rank.Two, Suit.Clubs)
        )
        HandEvaluator.evaluate(h).rank shouldBe HandRank.Flush
      }
    }

    "given a Straight" should {
      "return Straight" in {
        val h = makeHand(
          (Rank.Ten, Suit.Hearts), (Rank.Nine, Suit.Clubs),
          (Rank.Eight, Suit.Diamonds), (Rank.Seven, Suit.Spades), (Rank.Six, Suit.Hearts)
        )
        HandEvaluator.evaluate(h).rank shouldBe HandRank.Straight
      }

      "recognise the wheel (A-2-3-4-5) as a Straight" in {
        val h = makeHand(
          (Rank.Ace, Suit.Hearts), (Rank.Two, Suit.Clubs),
          (Rank.Three, Suit.Diamonds), (Rank.Four, Suit.Spades), (Rank.Five, Suit.Hearts)
        )
        HandEvaluator.evaluate(h).rank shouldBe HandRank.Straight
      }
    }

    "given Three of a Kind" should {
      "return ThreeOfAKind" in {
        val h = makeHand(
          (Rank.Queen, Suit.Hearts), (Rank.Queen, Suit.Diamonds), (Rank.Queen, Suit.Clubs),
          (Rank.King, Suit.Hearts), (Rank.Two, Suit.Spades)
        )
        HandEvaluator.evaluate(h).rank shouldBe HandRank.ThreeOfAKind
      }
    }

    "given Two Pair" should {
      "return TwoPair" in {
        val h = makeHand(
          (Rank.Ace, Suit.Hearts), (Rank.Ace, Suit.Diamonds),
          (Rank.King, Suit.Hearts), (Rank.King, Suit.Clubs),
          (Rank.Two, Suit.Spades)
        )
        HandEvaluator.evaluate(h).rank shouldBe HandRank.TwoPair
      }
    }

    "given One Pair" should {
      "return OnePair" in {
        val h = makeHand(
          (Rank.Jack, Suit.Hearts), (Rank.Jack, Suit.Spades),
          (Rank.Ace, Suit.Clubs), (Rank.King, Suit.Diamonds), (Rank.Two, Suit.Hearts)
        )
        HandEvaluator.evaluate(h).rank shouldBe HandRank.OnePair
      }
    }

    "given High Card" should {
      "return HighCard" in {
        val h = makeHand(
          (Rank.Ace, Suit.Hearts), (Rank.Jack, Suit.Clubs),
          (Rank.Nine, Suit.Diamonds), (Rank.Seven, Suit.Spades), (Rank.Two, Suit.Hearts)
        )
        HandEvaluator.evaluate(h).rank shouldBe HandRank.HighCard
      }
    }

    "given wrong card count" should {
      "throw IllegalArgumentException" in {
        an[IllegalArgumentException] should be thrownBy
          HandEvaluator.evaluate(Hand(List(Card(Rank.Ace, Suit.Hearts))))
      }
    }
  }


  "HandEvaluator.bestFiveFrom" should {

    "find a flush from 7 cards when one exists" in {
      // 5 clubs among 7 cards – best hand should be a flush
      val cards = List(
        Card(Rank.Ace,   Suit.Clubs),
        Card(Rank.King,  Suit.Clubs),
        Card(Rank.Queen, Suit.Clubs),
        Card(Rank.Jack,  Suit.Clubs),
        Card(Rank.Nine,  Suit.Clubs),
        Card(Rank.Two,   Suit.Hearts),
        Card(Rank.Three, Suit.Diamonds)
      )
      HandEvaluator.bestFiveFrom(cards).rank shouldBe HandRank.Flush
    }

    "find a straight from 7 cards" in {
      val cards = List(
        Card(Rank.Ten,   Suit.Hearts),
        Card(Rank.Nine,  Suit.Clubs),
        Card(Rank.Eight, Suit.Diamonds),
        Card(Rank.Seven, Suit.Spades),
        Card(Rank.Six,   Suit.Hearts),
        Card(Rank.Two,   Suit.Clubs),
        Card(Rank.Three, Suit.Diamonds)
      )
      HandEvaluator.bestFiveFrom(cards).rank shouldBe HandRank.Straight
    }

    "prefer a stronger hand over a weaker subset" in {
      // Full house available but also a pair – must choose full house
      val cards = List(
        Card(Rank.King, Suit.Hearts),
        Card(Rank.King, Suit.Diamonds),
        Card(Rank.King, Suit.Clubs),
        Card(Rank.Two,  Suit.Hearts),
        Card(Rank.Two,  Suit.Diamonds),
        Card(Rank.Ace,  Suit.Spades),
        Card(Rank.Jack, Suit.Clubs)
      )
      HandEvaluator.bestFiveFrom(cards).rank shouldBe HandRank.FullHouse
    }

    "throw when fewer than 5 cards are provided" in {
      an[IllegalArgumentException] should be thrownBy
        HandEvaluator.bestFiveFrom(List(Card(Rank.Ace, Suit.Hearts)))
    }
  }


  "HandRank ordering" should {
    "rank StraightFlush above Flush" in {
      HandRank.StraightFlush.value should be > HandRank.Flush.value
    }

    "rank FullHouse above Straight" in {
      HandRank.FullHouse.value should be > HandRank.Straight.value
    }

    "rank all hands in the correct ascending order" in {
      val ordered = List(
        HandRank.HighCard, HandRank.OnePair, HandRank.TwoPair,
        HandRank.ThreeOfAKind, HandRank.Straight, HandRank.Flush,
        HandRank.FullHouse, HandRank.FourOfAKind, HandRank.StraightFlush,
        HandRank.RoyalFlush
      )
      ordered shouldBe ordered.sorted
    }
  }


  "HandEvaluator.determineWinner" should {

    "return the player with the stronger hand" in {
      // Alice has a flush (all clubs in hole + community); Bob only has a pair
      val community = List(
        Card(Rank.Nine,  Suit.Clubs),
        Card(Rank.Seven, Suit.Clubs),
        Card(Rank.Two,   Suit.Clubs),
        Card(Rank.King,  Suit.Hearts),
        Card(Rank.Four,  Suit.Diamonds)
      )
      val alice = List(Card(Rank.Ace, Suit.Clubs), Card(Rank.Jack, Suit.Clubs))
      val bob   = List(Card(Rank.King, Suit.Spades), Card(Rank.King, Suit.Diamonds))

      val winners = HandEvaluator.determineWinner(List(
        ("Alice", alice, community),
        ("Bob",   bob,   community)
      ))
      winners.map(_._1) shouldBe List("Alice")
    }

    "return both players on an exact tie" in {
      // Both players use only the community cards (hole cards are irrelevant low cards)
      val community = List(
        Card(Rank.Ace,   Suit.Hearts),
        Card(Rank.King,  Suit.Hearts),
        Card(Rank.Queen, Suit.Hearts),
        Card(Rank.Jack,  Suit.Hearts),
        Card(Rank.Ten,   Suit.Hearts)
      )
      val alice = List(Card(Rank.Two, Suit.Clubs),    Card(Rank.Three, Suit.Clubs))
      val bob   = List(Card(Rank.Two, Suit.Diamonds), Card(Rank.Three, Suit.Diamonds))

      val winners = HandEvaluator.determineWinner(List(
        ("Alice", alice, community),
        ("Bob",   bob,   community)
      ))
      winners should have size 2
    }

    "use kicker to break a tie" in {
      val community = List(
        Card(Rank.Ace,  Suit.Clubs),
        Card(Rank.Ace,  Suit.Diamonds),
        Card(Rank.Five, Suit.Hearts),
        Card(Rank.Six,  Suit.Spades),
        Card(Rank.Two,  Suit.Clubs)
      )
      // Alice's King kicker beats Bob's Queen kicker
      val alice = List(Card(Rank.King,  Suit.Hearts), Card(Rank.Three, Suit.Hearts))
      val bob   = List(Card(Rank.Queen, Suit.Hearts), Card(Rank.Three, Suit.Clubs))

      val winners = HandEvaluator.determineWinner(List(
        ("Alice", alice, community),
        ("Bob",   bob,   community)
      ))
      winners.map(_._1) shouldBe List("Alice")
    }
  }
}
