package engine

import model._
import logic.HandEvaluator

import scala.io.StdIn
import scala.annotation.tailrec

/**
 * Console entry point – Texas Hold'em, arbitrary number of human and AI players.
 *
 * Texas Hold'em flow per round:
 *   1. Each player receives 2 hole cards (private)
 *   2. Pre-flop betting  (UTG acts first, i.e. seat after the big blind)
 *   3. Flop: 3 community cards revealed
 *   4. Flop betting     (SB acts first among active players)
 *   5. Turn: 1 community card revealed
 *   6. Turn betting
 *   7. River: 1 community card revealed
 *   8. River betting
 *   9. Showdown: best 5 from (2 hole + 5 community)
 *
 * Seats are ordered; dealerIndex rotates each round.
 *   SB  = (dealerIndex + 1) % n
 *   BB  = (dealerIndex + 2) % n
 *   UTG = (dealerIndex + 3) % n  (first to act pre-flop)
 *
 * Run with:  sbt "runMain engine.ConsoleGame"
 */
object ConsoleGame {

  private val StartingChips = 1000
  private val SmallBlind    = 10
  private val BigBlind      = 20

//  def main(args: Array[String]): Unit = {
//    println("=== Texas Hold'em ===")
//    println("Actions: f = fold  |  c = call/check  |  r <amount> = raise")
//    println()
//
//    val numHumans = askInt("How many human players? (0+)", min = 0)
//    val numAIs    = askInt("How many AI players?    (1+)", min = 1)
//
//    val humans = (1 to numHumans).map { i =>
//      Player(s"Player $i", StartingChips, Hand.empty, PlayerType.Human)
//    }.toList
//
//    val ais = (1 to numAIs).map { i =>
//      Player(s"Bot $i", StartingChips, Hand.empty, PlayerType.Computer)
//    }.toList
//
//    val players = humans ++ ais
//    println(s"\nStarting game with ${players.map(_.name).mkString(", ")}")
//    gameLoop(players, dealerIndex = 0, round = 1)
//  }

  // ─── Helpers ───────────────────────────────────────────────────────────────

  private def ask(prompt: String): String = {
    print(prompt + " -> ")
    val line = StdIn.readLine()
    Option(line).getOrElse("").trim
  }

  private def askInt(prompt: String, min: Int): Int = {
    @tailrec def loop(): Int = {
      val s = ask(prompt)
      s.toIntOption match {
        case Some(n) if n >= min => n
        case _ =>
          println(s"Please enter a number >= $min")
          loop()
      }
    }
    loop()
  }

  private def pause(msg: String = "[ Press Enter to continue ]"): Unit = {
    print(msg)
    StdIn.readLine()
  }

  // ─── Game loop ─────────────────────────────────────────────────────────────

  @tailrec
  private def gameLoop(players: List[Player], dealerIndex: Int, round: Int): Unit = {
    val active = players.filter(_.chips > 0)
    if (active.size < 2) {
      if (active.size == 1) println(s"\n${active.head.name} wins the game!")
      else println("\nNo players remaining.")
      return
    }

    println(s"\n─── Round $round ───  Dealer: ${players(dealerIndex % players.size).name}")
    players.foreach(p => println(s"  ${p.name}: ${p.chips} chips"))
    pause()

    val updatedPlayers = playRound(players, dealerIndex % players.size)

    val again = ask("\nPlay another round? (y / n)")
    if (again.equalsIgnoreCase("y")) {
      val nextDealer = nextActiveIndex(updatedPlayers, dealerIndex, updatedPlayers.size)
      gameLoop(updatedPlayers, nextDealer, round + 1)
    } else {
      println("Thanks for playing!")
    }
  }

  // Advance dealerIndex to the next player who still has chips
  private def nextActiveIndex(players: List[Player], current: Int, n: Int): Int = {
    val next = (current + 1) % n
    if (players(next).chips > 0) next else nextActiveIndex(players, next, n)
  }

  // ─── Single round ──────────────────────────────────────────────────────────

  private def playRound(allPlayers: List[Player], dealerIdx: Int): List[Player] = {
    val n = allPlayers.size

    // Reset folded state from previous round
    val freshPlayers  = allPlayers.map(p => if (p.folded) p.copy(folded = false) else p)

    // Seat indices that still have chips
    val seatOrder     = (0 until n).toList
    val eligibleSeats = seatOrder.filter(i => freshPlayers(i).chips > 0)

    if (eligibleSeats.size < 2) return freshPlayers

    // Find dealer seat within eligible list, then assign SB/BB
    val dealerPosInEligible = {
      val exact = eligibleSeats.indexOf(dealerIdx)
      if (exact >= 0) exact
      else eligibleSeats.indexWhere(_ > dealerIdx).max(0)
    }
    val sbSeat = eligibleSeats((dealerPosInEligible + 1) % eligibleSeats.size)
    val bbSeat = eligibleSeats((eligibleSeats.indexOf(sbSeat) + 1) % eligibleSeats.size)

    // Deduct blinds
    var players = freshPlayers.zipWithIndex.map { case (p, i) =>
      if (i == sbSeat) p.removeChips(SmallBlind.min(p.chips))
      else if (i == bbSeat) p.removeChips(BigBlind.min(p.chips))
      else p
    }

    val initialBets = Array.fill(n)(0)
    initialBets(sbSeat) = SmallBlind.min(freshPlayers(sbSeat).chips)
    initialBets(bbSeat) = BigBlind.min(freshPlayers(bbSeat).chips)

    var pot = initialBets.sum
    println(s"\nBlinds: ${freshPlayers(sbSeat).name} posts SB $SmallBlind, " +
            s"${freshPlayers(bbSeat).name} posts BB $BigBlind  |  Pot: $pot")

    // Deal hole cards
    var deck = Deck.standard.shuffle()
    players = players.zipWithIndex.map { case (p, i) =>
      if (eligibleSeats.contains(i)) {
        val (cards, rest) = deck.deal(2)
        deck = rest
        p.withHand(Hand(cards))
      } else p
    }

    // Show human hole cards
    players.zipWithIndex.foreach { case (p, i) =>
      if (p.playerType == PlayerType.Human && eligibleSeats.contains(i))
        println(s"\n${p.name}'s hole cards: ${p.hand}")
    }

    // UTG = first to act pre-flop (seat after BB)
    val utg = (eligibleSeats.indexOf(bbSeat) + 1) % eligibleSeats.size

    // Shared hand history — accumulated across streets
    var history = List.empty[ActionRecord]

    // Calculate positions for this round
    val seatPositions = calculatePositions(eligibleSeats, dealerIdx)

    // ── Pre-flop ──
    val (ps1, pot1, history1, ended1) = bettingRound(
      players, eligibleSeats, pot, currentBet = BigBlind,
      streetBets = initialBets.clone(), firstActIdx = utg,
      community = Nil, street = Street.PreFlop, history = history,
      seatPositions = seatPositions
    )
    players = ps1; pot = pot1; history = history1
    if (ended1 || activePlayers(players, eligibleSeats).size == 1)
      return awardPot(players, pot, eligibleSeats)

    // ── Flop ──
    val (flop, d2) = deck.deal(3); deck = d2
    val community1 = flop
    println(s"\n── Flop ──  Community: ${showCards(community1)}")
    showHoleCards(players, eligibleSeats)

    val (ps2, pot2, history2, ended2) = bettingRound(
      players, eligibleSeats, pot, currentBet = 0,
      streetBets = Array.fill(n)(0), firstActIdx = 0,
      community = community1, street = Street.Flop, history = history,
      seatPositions = seatPositions
    )
    players = ps2; pot = pot2; history = history2
    if (ended2 || activePlayers(players, eligibleSeats).size == 1)
      return awardPot(players, pot, eligibleSeats)

    // ── Turn ──
    val (turn, d3) = deck.deal(1); deck = d3
    val community2 = community1 ++ turn
    println(s"\n── Turn ──  Community: ${showCards(community2)}")
    showHoleCards(players, eligibleSeats)

    val (ps3, pot3, history3, ended3) = bettingRound(
      players, eligibleSeats, pot, currentBet = 0,
      streetBets = Array.fill(n)(0), firstActIdx = 0,
      community = community2, street = Street.Turn, history = history,
      seatPositions = seatPositions
    )
    players = ps3; pot = pot3; history = history3
    if (ended3 || activePlayers(players, eligibleSeats).size == 1)
      return awardPot(players, pot, eligibleSeats)

    // ── River ──
    val (river, _) = deck.deal(1)
    val community3 = community2 ++ river
    println(s"\n── River ── Community: ${showCards(community3)}")
    showHoleCards(players, eligibleSeats)

    val (ps4, pot4, _, ended4) = bettingRound(
      players, eligibleSeats, pot, currentBet = 0,
      streetBets = Array.fill(n)(0), firstActIdx = 0,
      community = community3, street = Street.River, history = history,
      seatPositions = seatPositions
    )
    players = ps4; pot = pot4
    if (ended4 || activePlayers(players, eligibleSeats).size == 1)
      return awardPot(players, pot, eligibleSeats)

    // ── Showdown ──
    showdown(players, eligibleSeats, pot4, community3)
  }

  // Returns the eligible seats that haven't folded
  private def activePlayers(players: List[Player], eligibleSeats: List[Int]): List[Int] =
    eligibleSeats.filter(i => !players(i).folded)

  // ─── Betting round ─────────────────────────────────────────────────────────
  //
  // `eligibleSeats`  – seats with chips at start of the hand (fixed)
  // `currentBet`     – the highest total bet anyone has made this street
  // `streetBets`     – how much each seat has put in this street so far
  // `firstActIdx`    – index into eligibleSeats of who acts first
  // `history`        – hand history so far (read-only input; returned extended)
  //
  // Returns (updatedPlayers, newPot, extendedHistory, roundEndedEarly)
  //
  private def bettingRound(
    players:       List[Player],
    eligibleSeats: List[Int],
    pot:           Int,
    currentBet:    Int,
    streetBets:    Array[Int],
    firstActIdx:   Int,
    community:     List[model.Card],
    street:        Street,
    history:       List[ActionRecord],
    seatPositions: Map[Int, String]
  ): (List[Player], Int, List[ActionRecord], Boolean) = {

    println(s"\n── $street betting ──")
    var ps          = players
    var currentBetV = currentBet
    var potV        = pot
    var historyV    = history
    val bets        = streetBets.clone()
    val numSeats    = eligibleSeats.size

    var lastAggressorIdx = (firstActIdx + numSeats - 1) % numSeats
    var actionIdx        = firstActIdx
    var actionsDone      = 0
    var keepGoing        = true

    while (keepGoing) {
      val seatI  = eligibleSeats(actionIdx % numSeats)
      val player = ps(seatI)

      if (!player.folded && player.chips > 0) {
        val toCall = (currentBetV - bets(seatI)).max(0).min(player.chips)

        val action = player.playerType match {
          case PlayerType.Human =>
            humanAction(player, pot = potV, toCall = toCall, community)

          case PlayerType.Computer =>
            val ctx = GameContext(
              street           = street,
              community        = community,
              holeCards        = player.hand,
              pot              = potV,
              toCall           = toCall,
              availableChips   = player.chips,
              currentBet       = currentBetV,
              numActivePlayers = activePlayers(ps, eligibleSeats).size,
              opponentStacks   = activePlayers(ps, eligibleSeats)
                                   .filterNot(i => ps(i).name == player.name)
                                   .map(i => ps(i).name -> ps(i).chips),
              position          = seatPositions.getOrElse(seatI, "Unknown"),
              opponentPositions = activePlayers(ps, eligibleSeats)
                                   .filterNot(i => ps(i).name == player.name)
                                   .map(i => ps(i).name -> seatPositions.getOrElse(i, "Unknown"))
                                   .toMap,
              history          = historyV
            )
            val a = ComputerAI.decideAction(ctx)
            println(s"${player.name} (${ctx.position}) thinks...  -> $a")
            // println(s"DEBUG: Opponents: ${ctx.opponentPositions}")
            a
        }

        // Resolve action
        val resolvedAction = action match {
          case Action.Fold =>
            ps = ps.updated(seatI, player.fold)
            println(s"${player.name} folds.")
            Action.Fold

          case Action.Call =>
            if (toCall == 0) {
              println(s"${player.name} checks.")
            } else {
              val actual = toCall.min(player.chips)
              ps     = ps.updated(seatI, player.removeChips(actual))
              bets(seatI) += actual
              potV   += actual
              println(s"${player.name} calls $actual.  Pot: $potV")
            }
            Action.Call

          case Action.Raise(amount) =>
            val legalMin  = currentBetV + 1
            val capped    = amount.max(legalMin).min(player.chips + bets(seatI))
            val additional = (capped - bets(seatI)).max(0)
            ps     = ps.updated(seatI, player.removeChips(additional))
            bets(seatI) += additional
            potV   += additional
            currentBetV = bets(seatI)
            lastAggressorIdx = actionIdx % numSeats
            println(s"${player.name} raises to $currentBetV.  Pot: $potV")
            Action.Raise(currentBetV)
        }

        // Record in hand history
        historyV = historyV :+ ActionRecord(player.name, street, resolvedAction, potV)
        actionsDone += 1

        if (resolvedAction == Action.Fold && activePlayers(ps, eligibleSeats).size == 1)
          return (ps, potV, historyV, true)
      }

      actionIdx += 1
      val nextNorm = actionIdx % numSeats
      if (actionsDone >= numSeats && nextNorm == (lastAggressorIdx + 1) % numSeats)
        keepGoing = false
      if (activePlayers(ps, eligibleSeats).size <= 1)
        keepGoing = false
    }

    (ps, potV, historyV, false)
  }

  // ─── Human input ───────────────────────────────────────────────────────────

  private def humanAction(player: Player, pot: Int, toCall: Int, community: List[model.Card]): Action = {
    if (community.nonEmpty)
      println(s"  Community: ${showCards(community)}")
    val callWord = if (toCall == 0) "check (c)" else s"call $toCall (c)"
    val raiseOpt = if (player.chips > toCall) "  |  raise <chips to add> (r 80)" else ""
    val prompt   = s"[${player.name}] chips:${player.chips}  pot:$pot  to-call:$toCall  |  fold(f)  $callWord$raiseOpt"

    @tailrec def readAction(): Action = {
      ask(prompt).toLowerCase match {
        case "f" => Action.Fold
        case "c" => Action.Call
        case s if s.startsWith("r ") =>
          val extra = s.drop(2).toIntOption.getOrElse(0)
          if (extra > toCall && extra <= player.chips) {
            Action.Raise(extra)
          } else {
            println(s"Invalid raise. Enter chips to add: must be > $toCall and <= ${player.chips}")
            readAction()
          }
        case _ =>
          println("Unknown action. Use: f / c / r <amount>")
          readAction()
      }
    }
    readAction()
  }

  // ─── Showdown ──────────────────────────────────────────────────────────────

  private def showdown(
    players: List[Player], eligibleSeats: List[Int], pot: Int, community: List[model.Card]
  ): List[Player] = {
    println("\n══ SHOWDOWN ══")
    println(s"Community: ${showCards(community)}")
    val active = activePlayers(players, eligibleSeats)
    active.foreach(i => println(s"  ${players(i).name}: ${players(i).hand}"))

    val entries = active.map { i =>
      val p = players(i)
      (p.name, p.hand.cards, community)
    }

    val winners = HandEvaluator.determineWinner(entries)
    println()
    if (winners.size > 1) {
      val share = pot / winners.size
      println(s"Tie! Pot of $pot split among ${winners.map(_._1).mkString(", ")} ($share each)")
      winners.foldLeft(players) { case (ps, (name, _)) =>
        val idx = eligibleSeats.find(i => players(i).name == name).getOrElse(-1)
        if (idx >= 0) ps.updated(idx, ps(idx).addChips(share)) else ps
      }
    } else {
      val (winnerName, evalHand) = winners.head
      println(s"$winnerName wins the pot of $pot with ${evalHand.rank.name}!")
      val winIdx = eligibleSeats.find(i => players(i).name == winnerName).getOrElse(-1)
      if (winIdx >= 0) players.updated(winIdx, players(winIdx).addChips(pot))
      else players
    }
  }

  // Award the pot to the sole remaining active player (everyone else folded)
  private def awardPot(players: List[Player], pot: Int, eligibleSeats: List[Int]): List[Player] = {
    val remaining = activePlayers(players, eligibleSeats)
    if (remaining.size == 1) {
      val idx    = remaining.head
      val winner = players(idx)
      println(s"\n${winner.name} wins the pot of $pot (all others folded)!")
      players.updated(idx, winner.addChips(pot))
    } else players
  }

  // ─── Display helpers ───────────────────────────────────────────────────────

  private def showCards(cards: List[model.Card]): String =
    cards.map(_.toString).mkString("  ")

  private def showHoleCards(players: List[Player], eligibleSeats: List[Int]): Unit =
    eligibleSeats.foreach { i =>
      val p = players(i)
      if (p.playerType == PlayerType.Human && !p.folded)
        println(s"  ${p.name}'s hole cards: ${p.hand}")
    }

  private def calculatePositions(eligibleSeats: List[Int], dealerIdx: Int): Map[Int, String] = {
    val n = eligibleSeats.size
    val dealerPos = eligibleSeats.indexOf(dealerIdx) match {
      case -1 =>
        // Find the first eligible seat after dealerIdx
        val after = eligibleSeats.indexWhere(_ > dealerIdx)
        if (after >= 0) after else 0
      case idx => idx
    }

    if (n == 2) {
      // Heads-up: Dealer is SB, other is BB
      // In ConsoleGame logic:
      // sbSeat = eligibleSeats((dealerPos + 1) % 2)
      // bbSeat = eligibleSeats((indexOf(sbSeat) + 1) % 2)
      // This means dealerPos is BB and (dealerPos + 1) is SB.
      // Let's align with the existing SB/BB logic in playRound.
      val sbIdx = eligibleSeats((dealerPos + 1) % n)
      val bbIdx = eligibleSeats((eligibleSeats.indexOf(sbIdx) + 1) % n)
      Map(sbIdx -> "SB/BTN", bbIdx -> "BB")
    } else {
      val positions = Map.newBuilder[Int, String]
      for (i <- 0 until n) {
        val seatIdx = eligibleSeats((dealerPos + i) % n)
        val posName = i match {
          case 0 => "BTN"
          case 1 => "SB"
          case 2 => "BB"
          case 3 => "UTG"
          case _ if i == n - 1 => "CO"
          case _ if i == n - 2 => "HJ"
          case _ => s"MP${i - 3}"
        }
        positions += (seatIdx -> posName)
      }
      positions.result()
    }
  }
}
