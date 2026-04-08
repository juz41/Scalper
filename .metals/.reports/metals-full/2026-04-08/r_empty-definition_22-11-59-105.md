error id: file:///C:/Users/Pitersoon/Desktop/Scala-final/poker-scala/app/engine/ConsoleGame.scala:
file:///C:/Users/Pitersoon/Desktop/Scala-final/poker-scala/app/engine/ConsoleGame.scala
empty definition using pc, found symbol in pc: 
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -model/h/hand.
	 -model/h/hand#
	 -model/h/hand().
	 -h/hand.
	 -h/hand#
	 -h/hand().
	 -scala/Predef.h.hand.
	 -scala/Predef.h.hand#
	 -scala/Predef.h.hand().
offset: 3059
uri: file:///C:/Users/Pitersoon/Desktop/Scala-final/poker-scala/app/engine/ConsoleGame.scala
text:
```scala
package engine

import model._
import logic.HandEvaluator

import scala.io.StdIn
import scala.annotation.tailrec

/**
 * Console entry point – Texas Hold'em, one human vs. one computer.
 *
 * Texas Hold'em flow per round:
 *   1. Each player receives 2 hole cards (private)
 *   2. Pre-flop betting
 *   3. Flop: 3 community cards revealed
 *   4. Flop betting
 *   5. Turn: 1 community card revealed
 *   6. Turn betting
 *   7. River: 1 community card revealed
 *   8. River betting
 *   9. Showdown: best 5 from (2 hole + 5 community)
 *
 * Run with:  sbt "runMain engine.ConsoleGame"
 */
object ConsoleGame {

  private val StartingChips = 1000
  private val SmallBlind    = 10
  private val BigBlind      = 20

  def main(args: Array[String]): Unit = {
    println("Texas Hold'em")
    println("Actions:")
    println("f = fold")
    println("c = call / check")
    println("r <amount> = raise")

    val human    = Player("You",      StartingChips, Hand.empty, PlayerType.Human)
    val computer = Player("Computer", StartingChips, Hand.empty, PlayerType.Computer)
    gameLoop(human, computer, round = 1)
  }


  private def ask(prompt: String): String = {
    println(prompt)
    val line = StdIn.readLine()
    Option(line).getOrElse("").trim
  }

  private def pause(msg: String = "[ Press Enter to continue ]"): Unit = {
    println(msg)
    StdIn.readLine()
  }

  // Game loop
  @tailrec
  private def gameLoop(human: Player, computer: Player, round: Int): Unit = {
    if (human.chips <= 0)    { println("\nYou are out of chips. Game over."); return }
    if (computer.chips <= 0) { println("\nComputer is out of chips. You win!"); return }

    println(s"\nRound $round  |  Your chips: ${human.chips}  |  Computer: ${computer.chips}")
    pause()

    val (h, c) = playRound(human, computer)

    val again = ask("\nPlay another round? (y / n) ->")
    if (again.equalsIgnoreCase("y"))
      gameLoop(h, c, round + 1)
    else
      println("Thanks for playing!")
  }

  // Single Texas Hold'em round

  private def playRound(human: Player, computer: Player): (Player, Player) = {

    // Blinds
    val h0   = human.removeChips(SmallBlind)
    val c0   = computer.removeChips(BigBlind)
    var pot  = SmallBlind + BigBlind
    println(s"\nBlinds posted. Pot: $pot  (You: small blind $SmallBlind, Computer: big blind $BigBlind)")

    // Deal hole cards
    val deck0           = Deck.standard.shuffle()
    val (hCards, deck1) = deck0.deal(2)
    val (cCards, deck2) = deck1.deal(2)
    var h = h0.withHand(Hand(hCards))
    var c = c0.withHand(Hand(cCards))

    println(s"\nYour hole cards:  ${h.hand}")
    println( "Computer's cards: [ ? ? ]\n")

    // ── Pre-flop ──
    println("Pre-flop betting")
    val (h1, c1, pot1, ended1) = bettingRound(h, c, pot, toCall = BigBlind - SmallBlind)
    if (ended1) return (h1, c1)
    h = h1; c = c1; pot = pot1

    // ── Flop ──
    val (flop, deck3)    = deck2.deal(3)
    var community        = flop
    println(s"\nFlop, Community: ${showCommunity(community)}")
    println(s"Your hand:  ${h.h@@and}\n")

    val (h2, c2, pot2, ended2) = bettingRound(h, c, pot, toCall = 0)
    if (ended2) return (h2, c2)
    h = h2; c = c2; pot = pot2

    // ── Turn ──
    val (turn, deck4) = deck3.deal(1)
    community         = community ++ turn
    println(s"\nTurn,  Community: ${showCommunity(community)}")
    println(s"Your hand:  ${h.hand}\n")

    val (h3, c3, pot3, ended3) = bettingRound(h, c, pot, toCall = 0)
    if (ended3) return (h3, c3)
    h = h3; c = c3; pot = pot3

    // ── River ──
    val (river, _) = deck4.deal(1)
    community      = community ++ river
    println(s"\n River,  Community: ${showCommunity(community)}")
    println(s"Your hand:  ${h.hand}\n")

    val (h4, c4, pot4, ended4) = bettingRound(h, c, pot, toCall = 0)
    if (ended4) return (h4, c4)

    // ── Showdown ──
    showdown(h4, c4, pot4, community)
  }

  // Betting round
  private def bettingRound(
    human: Player, computer: Player, pot: Int, toCall: Int
  ): (Player, Player, Int, Boolean) = {

    val action = ComputerAI.decideAction(toCall, computer.chips)
    println(s"Computer's action: $action")

    action match {
      case Action.Fold =>
        println("Computer folded - you win the pot!")
        (human.addChips(pot), computer, pot, true)

      case Action.Call =>
        val amount = toCall.min(computer.chips)
        val c      = computer.removeChips(amount)
        if (amount == 0) println("Computer checks.")
        else             println(s"Computer calls $amount.")
        humanTurn(human, c, pot + amount, toCall)

      case Action.Raise(amount) =>
        val c = computer.removeChips(amount)
        println(s"Computer raises to $amount.")
        humanTurn(human, c, pot + amount, amount)
    }
  }

  private def humanTurn(
    human: Player, computer: Player, pot: Int, toCall: Int
  ): (Player, Player, Int, Boolean) = {

    val callWord = if (toCall == 0) "check (c)" else s"call $toCall (c)"
    val raiseOpt = if (human.chips > toCall) "  |  raise <amount> (r 50)" else ""
    val prompt   =
      s"""Your chips: ${human.chips}  |  Pot: $pot  |  To call: $toCall
  Actions:  fold (f)  |  $callWord$raiseOpt
->""".stripMargin

    ask(prompt).toLowerCase match {
      case "f" =>
        println("You folded - computer wins the pot.")
        (human, computer.addChips(pot), pot, true)

      case s if s.startsWith("r ") =>
        val amount = s.drop(2).toIntOption.getOrElse(0)
        if (amount > toCall && amount <= human.chips) {
          println(s"You raise to $amount.")
          (human.removeChips(amount), computer, pot + amount, false)
        } else {
          println("Invalid raise amount - defaulting to call.")
          (human.removeChips(toCall), computer, pot, false)
        }

      case _ =>
        if (toCall == 0) println("You check.")
        else             println(s"You call $toCall.")
        (human.removeChips(toCall), computer, pot, false)
    }
  }

  // Showdown
  private def showdown(
    human: Player, computer: Player, pot: Int, community: List[model.Card]
  ): (Player, Player) = {
    println("\nSHOWDOWN")
    println(s"Community cards:  ${showCommunity(community)}")
    println(s"Your hole cards:  ${human.hand}")
    println(s"Computer's cards: ${computer.hand}")

    val winners = HandEvaluator.determineWinner(List(
      (human.name,    human.hand.cards,    community),
      (computer.name, computer.hand.cards, community)
    ))

    println("")
    if (winners.size > 1) {
      println(s"It's a tie! Pot of $pot is split.")
      val half = pot / 2
      (human.addChips(half), computer.addChips(half))
    } else if (winners.head._1 == human.name) {
      println(s"You win with ${winners.head._2.rank.name}!  (+$pot chips)")
      (human.addChips(pot), computer)
    } else {
      println(s"Computer wins with ${winners.head._2.rank.name}!  (+$pot to computer)")
      (human, computer.addChips(pot))
    }
  }

  // Display helpers
  private def showCommunity(cards: List[model.Card]): String =
    cards.map(_.toString).mkString("  ")
}

```


#### Short summary: 

empty definition using pc, found symbol in pc: 