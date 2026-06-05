package engine

import model._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GameEngineSpec extends AnyWordSpec with Matchers {

  private def card(rank: Rank, suit: Suit): Card = Card(rank, suit)

  private def player(
    name: String,
    chips: Int,
    hole: List[Card],
    folded: Boolean = false
  ): Player =
    Player(name, chips, Hand(hole), PlayerType.Computer, folded)

  private def baseState(
    players: List[Player],
    phase: GamePhase = GamePhase.AwaitingStart,
    dealerIdx: Int = 0,
    roundNum: Int = 1,
    pot: Int = 0,
    deck: Deck = Deck.standard,
    community: List[Card] = Nil,
    eligibleSeats: List[Int] = Nil,
    currentBet: Int = 0,
    streetBets: Map[Int, Int] = Map.empty,
    history: List[ActionRecord] = Nil
  ): GameState =
    GameState(
      players = players,
      dealerIdx = dealerIdx,
      roundNum = roundNum,
      phase = phase,
      pot = pot,
      deck = deck,
      community = community,
      eligibleSeats = eligibleSeats,
      currentBet = currentBet,
      streetBets = streetBets,
      history = history
    )

  "GameEngine.advance" should {
    "move from AwaitingStart to RoundInit when at least two active players remain" in {
      val s = baseState(
        players = List(
          player("Alice", 100, Nil),
          player("Bob", 120, Nil)
        )
      )

      val (next, events) = GameEngine.advance(s, connectedPlayers = Set("Alice", "Bob"))

      next.phase shouldBe GamePhase.RoundInit
      events shouldBe empty
    }

    "finish the game when only one active player remains" in {
      val s = baseState(
        players = List(
          player("Alice", 100, Nil),
          player("Bob", 0, Nil)
        )
      )

      val (next, events) = GameEngine.advance(s, connectedPlayers = Set("Alice", "Bob"))

      next.phase shouldBe GamePhase.GameOver
      events should contain(GameEvent.GameFinished("Alice wins the game!"))
    }

    "deal the flop and move into betting on the flop street" in {
      val fixedDeck = Deck(List(
        card(Rank.Two, Suit.Hearts),
        card(Rank.Three, Suit.Diamonds),
        card(Rank.Four, Suit.Clubs),
        card(Rank.Five, Suit.Spades),
        card(Rank.Six, Suit.Hearts)
      ))

      val s = baseState(
        players = List(
          player("Alice", 100, Nil),
          player("Bob", 100, Nil)
        ),
        phase = GamePhase.Flop,
        dealerIdx = 0,
        deck = fixedDeck,
        eligibleSeats = List(0, 1),
        community = Nil
      )

      val (next, events) = GameEngine.advance(s, connectedPlayers = Set("Alice", "Bob"))

      next.phase shouldBe GamePhase.Betting(Street.Flop, List(1, 0))
      next.community shouldBe List(
        card(Rank.Two, Suit.Hearts),
        card(Rank.Three, Suit.Diamonds),
        card(Rank.Four, Suit.Clubs)
      )
      next.deck.cards shouldBe List(
        card(Rank.Five, Suit.Spades),
        card(Rank.Six, Suit.Hearts)
      )
      events shouldBe List(GameEvent.StreetStarted(Street.Flop, next.community))
    }

    "resolve showdown, award the pot and reset the round" in {
      val community = List(
        card(Rank.Ace, Suit.Diamonds),
        card(Rank.King, Suit.Diamonds),
        card(Rank.Queen, Suit.Diamonds),
        card(Rank.Jack, Suit.Diamonds),
        card(Rank.Two, Suit.Clubs)
      )

      val s = baseState(
        players = List(
          player("Alice", 100, List(card(Rank.Ten, Suit.Diamonds), card(Rank.Nine, Suit.Clubs))),
          player("Bob", 100, List(card(Rank.Two, Suit.Spades), card(Rank.Three, Suit.Spades)))
        ),
        phase = GamePhase.Showdown,
        dealerIdx = 0,
        roundNum = 7,
        pot = 120,
        community = community,
        eligibleSeats = List(0, 1)
      )

      val (next, events) = GameEngine.advance(s, connectedPlayers = Set("Alice", "Bob"))

      next.phase shouldBe GamePhase.AwaitingStart
      next.roundNum shouldBe 8
      next.players(0).chips shouldBe 220
      next.players(1).chips shouldBe 100
      events should contain(GameEvent.PotWon(List(("Alice", 120)), Some("Royal Flush")))
    }
  }

  "GameEngine.processPlayerAction" should {
    "apply a call for the player whose turn it is" in {
      val s = baseState(
        players = List(
          player("Alice", 100, Nil),
          player("Bob", 100, Nil)
        ),
        phase = GamePhase.Betting(Street.PreFlop, List(0, 1)),
        pot = 30,
        currentBet = 20,
        streetBets = Map(0 -> 10, 1 -> 20),
        eligibleSeats = List(0, 1)
      )

      val result = GameEngine.processPlayerAction(s, "Alice", Action.Call)

      result.isRight shouldBe true
      val (next, events) = result.toOption.get

      next.players(0).chips shouldBe 90
      next.pot shouldBe 40
      next.streetBets(0) shouldBe 20
      next.phase shouldBe GamePhase.Betting(Street.PreFlop, List(1))
      events should contain(GameEvent.PlayerActed("Alice", Action.Call, 10, 40))
    }

    "reject actions from the wrong player or wrong phase" in {
      val s = baseState(
        players = List(
          player("Alice", 100, Nil),
          player("Bob", 100, Nil)
        )
      )

      GameEngine.processPlayerAction(s, "Alice", Action.Call) shouldBe Left("That's not your turn or wrong game phase.")
    }

    "apply a raise and rebuild the pending order" in {
      val s = baseState(
        players = List(
          player("Alice", 100, Nil),
          player("Bob", 100, Nil),
          player("Carol", 100, Nil)
        ),
        phase = GamePhase.Betting(Street.PreFlop, List(0, 1, 2)),
        pot = 40,
        currentBet = 20,
        streetBets = Map(0 -> 10, 1 -> 20, 2 -> 20),
        eligibleSeats = List(0, 1, 2)
      )

      val result = GameEngine.processPlayerAction(s, "Alice", Action.Raise(35))
      val (next, events) = result.toOption.get

      next.currentBet shouldBe 35
      next.pot shouldBe 65
      next.players(0).chips shouldBe 75
      next.streetBets(0) shouldBe 35
      next.phase shouldBe GamePhase.Betting(Street.PreFlop, List(1, 2))
      events should contain(GameEvent.PlayerActed("Alice", Action.Raise(35), 25, 65))
    }
  }
}
