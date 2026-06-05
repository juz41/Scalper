package model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PlayerSpec extends AnyWordSpec with Matchers {

  private def card(rank: Rank, suit: Suit): Card = Card(rank, suit)

  "Player" should {
    "create a player with the expected initial state" in {
      val player = Player(
        name = "Alice",
        chips = 100,
        hand = Hand.empty,
        playerType = PlayerType.Human
      )

      player.name shouldBe "Alice"
      player.chips shouldBe 100
      player.hand shouldBe Hand.empty
      player.playerType shouldBe PlayerType.Human
      player.folded shouldBe false
    }

    "support immutable hand update" in {
      val player = Player(
        name = "Alice",
        chips = 100,
        hand = Hand.empty,
        playerType = PlayerType.Human
      )

      val updated = player.withHand(
        Hand(List(card(Rank.Ace, Suit.Hearts), card(Rank.King, Suit.Spades)))
      )

      updated.hand.cards should have size 2
      updated.name shouldBe "Alice"
      updated.chips shouldBe 100
    }

    "support adding chips immutably" in {
      val player = Player("Alice", 100, Hand.empty, PlayerType.Human)

      player.addChips(25).chips shouldBe 125
    }

    "support removing chips immutably" in {
      val player = Player("Alice", 100, Hand.empty, PlayerType.Human)

      player.removeChips(40).chips shouldBe 60
    }

    "support folding" in {
      val player = Player("Alice", 100, Hand.empty, PlayerType.Human)

      player.fold.folded shouldBe true
    }

    "keep the original instance unchanged after updates" in {
      val player = Player("Alice", 100, Hand.empty, PlayerType.Human)
      val updated = player.addChips(50)

      player.chips shouldBe 100
      updated.chips shouldBe 150
    }
  }
}