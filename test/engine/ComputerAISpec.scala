package engine

import model._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ComputerAISpec extends AnyWordSpec with Matchers {

  private def card(rank: Rank, suit: Suit): Card = Card(rank, suit)

  private def ctx(
    street: Street = Street.PreFlop,
    pot: Int = 60,
    toCall: Int = 0,
    availableChips: Int = 200,
    currentBet: Int = 20,
    facingAggression: Boolean = false,
    raisesThisStreet: Int = 0
  ): GameContext = {
    val history =
      if (!facingAggression && raisesThisStreet == 0) Nil
      else List.fill(raisesThisStreet)(ActionRecord("Opponent", street, Action.Raise(currentBet), pot))

    GameContext(
      street = street,
      community = List(
        card(Rank.Ace, Suit.Hearts),
        card(Rank.King, Suit.Diamonds),
        card(Rank.Queen, Suit.Clubs)
      ),
      holeCards = Hand(List(card(Rank.Two, Suit.Spades), card(Rank.Three, Suit.Spades))),
      pot = pot,
      toCall = toCall,
      availableChips = availableChips,
      currentBet = currentBet,
      numActivePlayers = 3,
      opponentStacks = List("Bob" -> 90, "Carol" -> 70),
      position = "BTN",
      opponentPositions = Map("Bob" -> "SB", "Carol" -> "BB"),
      history = history
    )
  }

  "ComputerAI.decideAction" should {
    "never fold for a free check" in {
      val action = ComputerAI.decideAction(ctx(toCall = 0, facingAggression = false))

      action should not be Action.Fold
    }

    "never attempt a raise when the stack is too short to beat the current bet" in {
      val action = ComputerAI.decideAction(ctx(toCall = 50, availableChips = 40, street = Street.River))

      action match {
        case Action.Raise(_) => fail("raise should not be possible when available chips are not above the call amount")
        case Action.Call | Action.Fold => succeed
      }
    }

    "always return a legal action shape" in {
      val action = ComputerAI.decideAction(ctx(toCall = 10, availableChips = 120, street = Street.Flop))

      action match {
        case Action.Fold => succeed
        case Action.Call => succeed
        case Action.Raise(total) =>
          total should be >= 11
          total should be <= 120
      }
    }
  }
}
