package engine

import model._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GameContextSpec extends AnyWordSpec with Matchers {

  private def card(rank: Rank, suit: Suit): Card = Card(rank, suit)

  private def ctx(
    street: Street = Street.Flop,
    pot: Int = 100,
    toCall: Int = 20,
    availableChips: Int = 150,
    history: List[ActionRecord] = Nil,
    opponentStacks: List[(String, Int)] = List("Bob" -> 80, "Carol" -> 45)
  ): GameContext =
    GameContext(
      street = street,
      community = List(
        card(Rank.Ace, Suit.Hearts),
        card(Rank.King, Suit.Hearts),
        card(Rank.Queen, Suit.Hearts)
      ),
      holeCards = Hand(List(card(Rank.Two, Suit.Clubs), card(Rank.Three, Suit.Diamonds))),
      pot = pot,
      toCall = toCall,
      availableChips = availableChips,
      currentBet = 40,
      numActivePlayers = 3,
      opponentStacks = opponentStacks,
      position = "BTN",
      opponentPositions = Map("Bob" -> "SB", "Carol" -> "BB"),
      history = history
    )

  "GameContext" should {
    "compute total pot commitment" in {
      ctx(pot = 90, toCall = 15).totalPotCommitment shouldBe 105
    }

    "filter history by the current street" in {
      val history = List(
        ActionRecord("Alice", Street.PreFlop, Action.Call, 20),
        ActionRecord("Bob", Street.Flop, Action.Raise(60), 80),
        ActionRecord("Carol", Street.Flop, Action.Call, 80)
      )

      ctx(history = history).currentStreetHistory.map(_.playerName) shouldBe List("Bob", "Carol")
    }

    "count raises only on the current street" in {
      val history = List(
        ActionRecord("Alice", Street.PreFlop, Action.Raise(30), 30),
        ActionRecord("Bob", Street.Flop, Action.Raise(60), 80),
        ActionRecord("Carol", Street.Flop, Action.Call, 80),
        ActionRecord("Dave", Street.Flop, Action.Raise(100), 180)
      )

      ctx(history = history).raisesThisStreet shouldBe 2
    }

    "detect aggression when there was at least one raise on the street" in {
      ctx(history = List(ActionRecord("Bob", Street.Flop, Action.Raise(60), 80))).facingAggression shouldBe true
      ctx(history = List(ActionRecord("Bob", Street.PreFlop, Action.Raise(30), 30))).facingAggression shouldBe false
    }

    "compute pot odds correctly" in {
      ctx(pot = 100, toCall = 25).potOdds shouldBe 0.2 +- 0.0001
      ctx(toCall = 0).potOdds shouldBe 0.0
    }

    "find the shortest opponent stack" in {
      ctx(opponentStacks = List("Bob" -> 80, "Carol" -> 45, "Dave" -> 120)).shortestOpponentStack shouldBe Some(45)
      ctx(opponentStacks = Nil).shortestOpponentStack shouldBe None
    }
  }
}
