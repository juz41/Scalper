package engine

import model._
import logic.HandEvaluator
import scala.annotation.tailrec

/**
 * Engine interface for communication with external world
 * (e.g. with network, terminal or test environment)
 */
trait EngineIO {
  def broadcast(msg: String): Unit
  def send(playerName: String, msg: String): Unit
  def ask(playerName: String, prompt: String): String
  def isConnected(playerName: String): Boolean
  def fetchNewPlayers(): List[Player]
  def isGameActive: Boolean
  def onGameEnded(): Unit
}

/**
 * game entry point – Texas Hold'em, arbitrary number of human and AI players.
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
 */
class GameEngine(io: EngineIO, initialPlayers: List[Player]) {

  //private val StartingChips = 1000
  private val SmallBlind    = 10
  private val BigBlind      = 20

  def start(): Unit = {
    gameLoop(initialPlayers, dealerIndex = 0, round = 1)
  }

  // ─── Game loop ─────────────────────────────────────────────────────────────
  @tailrec
  private def gameLoop(currentPlayers: List[Player], dealerIndex: Int, round: Int): Unit = {
    if (!io.isGameActive) return

    val newPlayers = io.fetchNewPlayers()
    val players = currentPlayers ++ newPlayers
    val active = players.filter(p => p.chips > 0 && isPlayerConnected(p))

    if (active.size < 2) {
      if (active.size == 1) io.broadcast(s"\n${active.head.name} wins the game!")
      else io.broadcast("\nNo players remaining.")
      io.onGameEnded()
      return
    }

    io.broadcast(s"\n─── Round $round ───  Dealer: ${players(dealerIndex % players.size).name}")
    players.foreach { p =>
      val status = if(!isPlayerConnected(p)) " (Disconnected)" else ""
      io.broadcast(s"  ${p.name}: ${p.chips} chips $status")
    }

    io.broadcast("[Start in 5 seconds... Last chance for /bot]")
    Thread.sleep(5000)

    val updatedPlayers = playRound(players, dealerIndex % players.size)
    if (!io.isGameActive) return

    val nextDealer = nextActiveIndex(updatedPlayers, dealerIndex, updatedPlayers.size)
    gameLoop(updatedPlayers, nextDealer, round + 1)
  }

  // Check if player is connected or is it a bot
  private def isPlayerConnected(p: Player): Boolean = {
    p.playerType == PlayerType.Computer || io.isConnected(p.name)
  }

  // Advance dealerIndex to the next player who still has chips
  @tailrec
  private def nextActiveIndex(players: List[Player], current: Int, n: Int): Int = {
    val next = (current + 1) % n
    if (players(next).chips > 0 && isPlayerConnected(players(next))) next
    else nextActiveIndex(players, next, n)
  }

  // ─── Single round ──────────────────────────────────────────────────────────
  private def playRound(allPlayers: List[Player], dealerIdx: Int): List[Player] = {
    val n = allPlayers.size
    val freshPlayers = allPlayers.map(p => if (p.folded) p.copy(folded = false) else p)
    val seatOrder = (0 until n).toList
    val eligibleSeats = seatOrder.filter(i => freshPlayers(i).chips > 0)

    if (eligibleSeats.size < 2) return freshPlayers

    val dealerPosInEligible = {
      val exact = eligibleSeats.indexOf(dealerIdx)
      if (exact >= 0) exact else eligibleSeats.indexWhere(_ > dealerIdx).max(0)
    }

    val sbSeat = eligibleSeats((dealerPosInEligible + 1) % eligibleSeats.size)
    val bbSeat = eligibleSeats((eligibleSeats.indexOf(sbSeat) + 1) % eligibleSeats.size)

    var players = freshPlayers.zipWithIndex.map { case (p, i) =>
      if (i == sbSeat) p.removeChips(SmallBlind.min(p.chips))
      else if (i == bbSeat) p.removeChips(BigBlind.min(p.chips))
      else p
    }

    val initialBets = Array.fill(n)(0)
    initialBets(sbSeat) = SmallBlind.min(freshPlayers(sbSeat).chips)
    initialBets(bbSeat) = BigBlind.min(freshPlayers(bbSeat).chips)

    var pot = initialBets.sum
    io.broadcast(s"\nBlinds: ${freshPlayers(sbSeat).name} posts SB $SmallBlind, ${freshPlayers(bbSeat).name} posts BB $BigBlind  | Pot: $pot")

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
    showHoleCards(players, eligibleSeats)

    // UTG = first to act pre-flop (seat after BB)
    val utg = (eligibleSeats.indexOf(bbSeat) + 1) % eligibleSeats.size

    // Shared hand history — accumulated across streets
    var history = List.empty[ActionRecord]

    // Calculate positions for this round
    val seatPositions = calculatePositions(eligibleSeats, dealerIdx)

    // Pre-flop
    val (ps1, pot1, history1, ended1) = bettingRound(players, eligibleSeats, pot, BigBlind, initialBets.clone(), utg, Nil, Street.PreFlop, history, seatPositions)
    players = ps1; pot = pot1; history = history1
    if (ended1 || activePlayers(players, eligibleSeats).size == 1) return awardPot(players, pot, eligibleSeats)

    // Flop
    val (flop, d2) = deck.deal(3); deck = d2
    val community1 = flop
    io.broadcast(s"\n── Flop ──  Community: ${showCards(community1)}")
    showHoleCards(players, eligibleSeats)

    val (ps2, pot2, history2, ended2) = bettingRound(players, eligibleSeats, pot, 0, Array.fill(n)(0), 0, community1, Street.Flop, history, seatPositions)
    players = ps2; pot = pot2; history = history2
    if (ended2 || activePlayers(players, eligibleSeats).size == 1) return awardPot(players, pot, eligibleSeats)

    // Turn
    val (turn, d3) = deck.deal(1); deck = d3
    val community2 = community1 ++ turn
    io.broadcast(s"\n── Turn ──  Community: ${showCards(community2)}")
    showHoleCards(players, eligibleSeats)

    val (ps3, pot3, history3, ended3) = bettingRound(players, eligibleSeats, pot, 0, Array.fill(n)(0), 0, community2, Street.Turn, history, seatPositions)
    players = ps3; pot = pot3; history = history3
    if (ended3 || activePlayers(players, eligibleSeats).size == 1) return awardPot(players, pot, eligibleSeats)

    // River
    val (river, _) = deck.deal(1)
    val community3 = community2 ++ river
    io.broadcast(s"\n── River ── Community: ${showCards(community3)}")
    showHoleCards(players, eligibleSeats)

    val (ps4, pot4, _, ended4) = bettingRound(players, eligibleSeats, pot, 0, Array.fill(n)(0), 0, community3, Street.River, history, seatPositions)
    players = ps4; pot = pot4
    if (ended4 || activePlayers(players, eligibleSeats).size == 1) return awardPot(players, pot, eligibleSeats)

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
  private def bettingRound(players: List[Player], eligibleSeats: List[Int], pot: Int, currentBet: Int, streetBets: Array[Int], firstActIdx: Int, community: List[model.Card], street: Street, history: List[ActionRecord], seatPositions: Map[Int, String]): (List[Player], Int, List[ActionRecord], Boolean) = {
    io.broadcast(s"\n── $street betting ──")
    var ps = players
    var currentBetV = currentBet
    var potV = pot
    var historyV = history
    val bets = streetBets.clone()
    val numSeats = eligibleSeats.size

    var lastAggressorIdx = (firstActIdx + numSeats - 1) % numSeats
    var actionIdx = firstActIdx
    var actionsDone = 0
    var keepGoing = true

    while (keepGoing) {
      if (!io.isGameActive) return (ps, potV, historyV, true)

      val seatI = eligibleSeats(actionIdx % numSeats)
      val player = ps(seatI)

      if (!player.folded && player.chips > 0) {
        val toCall = (currentBetV - bets(seatI)).max(0).min(player.chips)

        val action = player.playerType match {
          case PlayerType.Human =>
            if (io.isConnected(player.name)) {
              humanAction(player, potV, toCall)
            } else {
              io.broadcast(s"[${player.name}] lost connection. Automatc Fold.")
              Action.Fold
            }
          case PlayerType.Computer =>
            val ctx = GameContext(street, community, player.hand, potV, toCall, player.chips, currentBetV, activePlayers(ps, eligibleSeats).size, activePlayers(ps, eligibleSeats).filterNot(i => ps(i).name == player.name).map(i => ps(i).name -> ps(i).chips), seatPositions.getOrElse(seatI, "Unknown"), activePlayers(ps, eligibleSeats).filterNot(i => ps(i).name == player.name).map(i => ps(i).name -> seatPositions.getOrElse(i, "Unknown")).toMap, historyV)
            val a = ComputerAI.decideAction(ctx)
            io.broadcast(s"${player.name} (${ctx.position}) thinks...  -> $a")
            Thread.sleep(1500)
            a
        }

        // Resolve action
        val resolvedAction = action match {
          case Action.Fold =>
            ps = ps.updated(seatI, player.fold)
            if(player.playerType == PlayerType.Human) io.broadcast(s"${player.name} folds.")
            Action.Fold
          case Action.Call =>
            if (toCall == 0) {
              io.broadcast(s"${player.name} checks.")
            } else {
              val actual = toCall.min(player.chips)
              ps = ps.updated(seatI, player.removeChips(actual))
              bets(seatI) += actual
              potV += actual
              io.broadcast(s"${player.name} calls $actual. Pot: $potV")
            }
            Action.Call
          case Action.Raise(amount) =>
            val legalMin = currentBetV + 1
            val capped = amount.max(legalMin).min(player.chips + bets(seatI))
            val additional = (capped - bets(seatI)).max(0)
            ps = ps.updated(seatI, player.removeChips(additional))
            bets(seatI) += additional
            potV += additional
            currentBetV = bets(seatI)
            lastAggressorIdx = actionIdx % numSeats
            io.broadcast(s"${player.name} raises to $currentBetV.  Pot: $potV")
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
      if (actionsDone >= numSeats && nextNorm == (lastAggressorIdx + 1) % numSeats) keepGoing = false
      if (activePlayers(ps, eligibleSeats).size <= 1) keepGoing = false
    }

    (ps, potV, historyV, false)
  }

  // ─── Human input ───────────────────────────────────────────────────────────
  private def humanAction(player: Player, pot: Int, toCall: Int): Action = {
    val callWord = if (toCall == 0) "check (c)" else s"call $toCall (c)"
    val raiseOpt = if (player.chips > toCall) "  | raise <chips to add> (r 80)" else ""
    val prompt = s"[Your move] chips:${player.chips}  pot:$pot  to-call:$toCall  | fold(f)  $callWord$raiseOpt"

    @tailrec def readAction(): Action = {
      val rawInput = io.ask(player.name, prompt).toLowerCase
      if (rawInput.startsWith("/")) {
        io.send(player.name, "Commands during turn ignored by engine (enter f, c or r) have been processed by lobby.")
        readAction()
      } else {
        rawInput match {
          case "f" => Action.Fold
          case "c" => Action.Call
          case s if s.startsWith("r ") =>
            val extra = s.drop(2).toIntOption.getOrElse(0)
            if (extra > toCall && extra <= player.chips) Action.Raise(extra)
            else {
              io.send(player.name, s"Invalid raise. Enter chips to add: must be > ${toCall + 1} and <= ${player.chips}")
              readAction()
            }
          case _ =>
            io.send(player.name, "Unknown action. Use: f / c / r <amount>")
            readAction()
        }
      }
    }
    readAction()
  }

  // ─── Showdown ──────────────────────────────────────────────────────────────
  private def showdown(players: List[Player], eligibleSeats: List[Int], pot: Int, community: List[model.Card]): List[Player] = {
    io.broadcast("\n══ SHOWDOWN ══")
    io.broadcast(s"Community: ${showCards(community)}")
    val active = activePlayers(players, eligibleSeats)
    active.foreach(i => io.broadcast(s"  ${players(i).name}: ${players(i).hand}"))

    val entries = active.map { i =>
      val p = players(i)
      (p.name, p.hand.cards, community)
    }

    val winners = HandEvaluator.determineWinner(entries)
    io.broadcast("")
    if (winners.size > 1) {
      val share = pot / winners.size
      io.broadcast(s"Tie! Pot of $pot split among ${winners.map(_._1).mkString(", ")} ($share each)")
      winners.foldLeft(players) { case (ps, (name, _)) =>
        val idx = eligibleSeats.find(i => players(i).name == name).getOrElse(-1)
        if (idx >= 0) ps.updated(idx, ps(idx).addChips(share)) else ps
      }
    } else {
      val (winnerName, evalHand) = winners.head
      io.broadcast(s"$winnerName wins the pot of $pot with ${evalHand.rank.name}!")
      val winIdx = eligibleSeats.find(i => players(i).name == winnerName).getOrElse(-1)
      if (winIdx >= 0) players.updated(winIdx, players(winIdx).addChips(pot)) else players
    }
  }

  // Award the pot to the sole remaining active player (everyone else folded)
  private def awardPot(players: List[Player], pot: Int, eligibleSeats: List[Int]): List[Player] = {
    val remaining = activePlayers(players, eligibleSeats)
    if (remaining.size == 1) {
      val idx = remaining.head
      val winner = players(idx)
      io.broadcast(s"\n${winner.name} wins the pot of $pot (all others folded)!")
      players.updated(idx, winner.addChips(pot))
    } else players
  }

  // ─── Display helpers ───────────────────────────────────────────────────────
  private def showCards(cards: List[model.Card]): String = cards.map(_.toString).mkString("  ")

  private def showHoleCards(players: List[Player], eligibleSeats: List[Int]): Unit = {
    eligibleSeats.foreach { i =>
      val p = players(i)
      if (p.playerType == PlayerType.Human && !p.folded && isPlayerConnected(p)) {
        io.send(p.name, s"  [SECRET] Your hole cards: ${p.hand}")
      }
    }
  }

  private def calculatePositions(eligibleSeats: List[Int], dealerIdx: Int): Map[Int, String] = {
    val n = eligibleSeats.size
    val dealerPos = eligibleSeats.indexOf(dealerIdx) match {
      // Find the first eligible seat after dealerIdx
      case -1 => val after = eligibleSeats.indexWhere(_ > dealerIdx); if (after >= 0) after else 0
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