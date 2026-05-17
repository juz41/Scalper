package engine

import model._
import logic.HandEvaluator
import scala.annotation.tailrec

// ─── Zdarzenia Domenowe (Fakty z gry, NIE instrukcje IO) ─────────────────────
sealed trait GameEvent
object GameEvent {
  case class GameFinished(reason: String) extends GameEvent
  case class RoundStarted(roundNum: Int, dealerName: String, playerChips: List[(String, Int, Boolean)]) extends GameEvent
  case class BlindsPosted(sbPlayer: String, sbAmount: Int, bbPlayer: String, bbAmount: Int) extends GameEvent
  case class HoleCardsDealt(playerSecrets: Map[String, Hand]) extends GameEvent
  case class StreetStarted(street: Street, communityCards: List[Card]) extends GameEvent
  case class ActionRequested(playerName: String, toCall: Int, pot: Int, availableChips: Int) extends GameEvent
  case class PlayerActed(playerName: String, action: Action, potAdded: Int, totalPot: Int) extends GameEvent
  case class PlayerDisconnected(playerName: String) extends GameEvent
  case class ShowdownRevealed(community: List[Card], hands: List[(String, Hand)]) extends GameEvent
  case class PotWon(winners: List[(String, Int)], handName: Option[String]) extends GameEvent
  case class PlayerBusted(playerName: String) extends GameEvent
}

// ─── Fazy Gry ────────────────────────────────────────────────────────────────
sealed trait GamePhase
object GamePhase {
  case object AwaitingStart extends GamePhase
  case object RoundInit extends GamePhase
  case object PreFlop extends GamePhase
  case object Flop extends GamePhase
  case object Turn extends GamePhase
  case object River extends GamePhase
  case class Betting(street: Street, pendingSeats: List[Int]) extends GamePhase
  case object Showdown extends GamePhase
  case object GameOver extends GamePhase
}

// ─── Stan Gry ────────────────────────────────────────────────────────────────
case class GameState(
                      players: List[Player],
                      dealerIdx: Int = 0,
                      roundNum: Int = 1,
                      phase: GamePhase = GamePhase.AwaitingStart,
                      pot: Int = 0,
                      deck: Deck = Deck.standard,
                      community: List[Card] = Nil,
                      eligibleSeats: List[Int] = Nil,
                      currentBet: Int = 0,
                      streetBets: Map[Int, Int] = Map.empty,
                      history: List[ActionRecord] = Nil,
                      seatPositions: Map[Int, String] = Map.empty
                    )

// ─── Silnik Pokerowy (Czysto Funkcyjny) ──────────────────────────────────────
object GameEngine {
  private val SmallBlind = 10
  private val BigBlind = 20

  /**
   * Automatycznie przesuwa grę do przodu, generując nowy stan i zdarzenia.
   * Jeśli gra wymaga interakcji gracza (faza Betting), zatrzymuje się i zwraca zdarzenie ActionRequested.
   */
  def advance(s: GameState, connectedPlayers: Set[String]): (GameState, List[GameEvent]) = {
    s.phase match {
      case GamePhase.AwaitingStart =>
        val active = s.players.filter(p => p.chips > 0 && isConnected(p, connectedPlayers))
        if (active.size < 2) {
          val reason = if (active.size == 1) s"${active.head.name} wins the game!" else "No players remaining."
          (s.copy(phase = GamePhase.GameOver), List(GameEvent.GameFinished(reason)))
        } else {
          (s.copy(phase = GamePhase.RoundInit), Nil)
        }

      case GamePhase.RoundInit =>
        val freshPlayers = s.players.map(p => if (p.folded) p.copy(folded = false) else p)
        val eligible = freshPlayers.indices.filter(i => freshPlayers(i).chips > 0).toList

        val dealerPos = eligible.indexOf(s.dealerIdx % s.players.size) match {
          case -1 => eligible.indexWhere(_ > (s.dealerIdx % s.players.size)).max(0)
          case exact => exact
        }

        val sbSeat = eligible((dealerPos + 1) % eligible.size)
        val bbSeat = eligible((eligible.indexOf(sbSeat) + 1) % eligible.size)

        var ps = freshPlayers
        ps = ps.updated(sbSeat, ps(sbSeat).removeChips(SmallBlind))
        ps = ps.updated(bbSeat, ps(bbSeat).removeChips(BigBlind))

        val dealerName = ps(s.dealerIdx % ps.size).name
        val chipsInfo = ps.map(p => (p.name, p.chips, isConnected(p, connectedPlayers)))
        var events: List[GameEvent] = List(
          GameEvent.RoundStarted(s.roundNum, dealerName, chipsInfo),
          GameEvent.BlindsPosted(ps(sbSeat).name, SmallBlind, ps(bbSeat).name, BigBlind)
        )

        var deck = Deck.standard.shuffle()
        ps = ps.zipWithIndex.map { case (p, i) =>
          if (eligible.contains(i)) {
            val (c, rest) = deck.deal(2)
            deck = rest
            p.withHand(Hand(c))
          } else p
        }

        val secrets = eligible.map(i => ps(i).name -> ps(i).hand).toMap
        events :+= GameEvent.HoleCardsDealt(secrets)

        val utg = eligible((eligible.indexOf(bbSeat) + 1) % eligible.size)
        val pending = orderedSeatsFrom(eligible, utg, includeStart = true, ps)

        val nextState = s.copy(
          players = ps, deck = deck, pot = SmallBlind + BigBlind, eligibleSeats = eligible,
          currentBet = BigBlind, streetBets = Map(sbSeat -> SmallBlind, bbSeat -> BigBlind),
          phase = GamePhase.Betting(Street.PreFlop, pending), community = Nil, history = Nil
        )

        (nextState, events)

      case GamePhase.Betting(street, pendingSeats) =>
        if (activePlayers(s.players, s.eligibleSeats).size == 1) {
          awardPot(s)
        } else if (pendingSeats.isEmpty) {
          val nextPhase = street match {
            case Street.PreFlop => GamePhase.Flop
            case Street.Flop => GamePhase.Turn
            case Street.Turn => GamePhase.River
            case Street.River => GamePhase.Showdown
          }
          (s.copy(phase = nextPhase), Nil)
        } else {
          val seatI = pendingSeats.head
          val p = s.players(seatI)

          if (p.folded || p.chips <= 0) {
            (s.copy(phase = GamePhase.Betting(street, pendingSeats.tail)), Nil)
          } else if (p.playerType == PlayerType.Human && !isConnected(p, connectedPlayers)) {
            // Automatyczny fold dla rozłączonego
            applyAction(s, seatI, Action.Fold, isForced = true)
          } else {
            val toCall = (s.currentBet - s.streetBets.getOrElse(seatI, 0)).max(0).min(p.chips)
            // Zwracamy ten sam stan, ale z żądaniem akcji (System czeka)
            (s, List(GameEvent.ActionRequested(p.name, toCall, s.pot, p.chips)))
          }
        }

      case GamePhase.Flop =>
        val (c, d) = s.deck.deal(3)
        startStreet(s, Street.Flop, s.community ++ c, d)

      case GamePhase.Turn =>
        val (c, d) = s.deck.deal(1)
        startStreet(s, Street.Turn, s.community ++ c, d)

      case GamePhase.River =>
        val (c, d) = s.deck.deal(1)
        startStreet(s, Street.River, s.community ++ c, d)

      case GamePhase.Showdown =>
        val active = activePlayers(s.players, s.eligibleSeats)
        val hands = active.map(i => s.players(i).name -> s.players(i).hand)

        var events: List[GameEvent] = List(GameEvent.ShowdownRevealed(s.community, hands))

        val entries = active.map(i => (s.players(i).name, s.players(i).hand.cards, s.community))
        val winners = HandEvaluator.determineWinner(entries)

        var ps = s.players
        val winShares = winners.map { case (name, h) =>
          val share = s.pot / winners.size
          val idx = s.eligibleSeats.find(i => ps(i).name == name).get
          ps = ps.updated(idx, ps(idx).addChips(share))
          (name, share)
        }

        events :+= GameEvent.PotWon(winShares, Some(winners.head._2.rank.name))

        val (bustedEvents, finalPlayers) = handleBusted(s.players, ps)
        val nextDealer = nextActiveIndex(finalPlayers, s.dealerIdx % finalPlayers.size, finalPlayers.size)

        val ns = s.copy(players = finalPlayers, phase = GamePhase.AwaitingStart, roundNum = s.roundNum + 1, dealerIdx = nextDealer)
        (ns, events ++ bustedEvents)

      case GamePhase.GameOver => (s, Nil)
    }
  }

  /**
   * Wywoływana gdy warstwa sieciowa dostarczy decyzję gracza.
   */
  def processPlayerAction(s: GameState, playerName: String, action: Action): Either[String, (GameState, List[GameEvent])] = {
    s.phase match {
      case GamePhase.Betting(street, pending) if pending.nonEmpty && s.players(pending.head).name == playerName =>
        Right(applyAction(s, pending.head, action, isForced = false))
      case _ =>
        Left("To nie jest twój ruch lub zła faza gry.")
    }
  }

  // Wewnętrzna aplikacja akcji i aktualizacja puli/historii
  private def applyAction(s: GameState, seatI: Int, action: Action, isForced: Boolean): (GameState, List[GameEvent]) = {
    val p = s.players(seatI)
    val betSoFar = s.streetBets.getOrElse(seatI, 0)
    var ps = s.players
    var pot = s.pot
    var currentBet = s.currentBet
    var bets = s.streetBets

    val sPhase = s.phase.asInstanceOf[GamePhase.Betting]
    var pending = sPhase.pendingSeats.tail

    val resolved = action match {
      case Action.Fold =>
        ps = ps.updated(seatI, p.fold)
        Action.Fold
      case Action.Call =>
        val toCall = (currentBet - betSoFar).max(0).min(p.chips)
        ps = ps.updated(seatI, p.removeChips(toCall))
        bets = bets.updated(seatI, betSoFar + toCall)
        pot += toCall
        Action.Call
      case Action.Raise(amount) =>
        val capped = amount.max(currentBet + 1).min(p.chips + betSoFar)
        val added = (capped - betSoFar).max(0)
        ps = ps.updated(seatI, p.removeChips(added))
        bets = bets.updated(seatI, betSoFar + added)
        pot += added
        currentBet = bets(seatI)
        pending = orderedSeatsFrom(s.eligibleSeats, seatI, includeStart = false, ps)
        Action.Raise(currentBet)
    }

    val history = s.history :+ ActionRecord(p.name, sPhase.street, resolved, pot)
    var events: List[GameEvent] = Nil
    if (isForced) events :+= GameEvent.PlayerDisconnected(p.name)
    events :+= GameEvent.PlayerActed(p.name, resolved, if (resolved == Action.Fold) 0 else bets(seatI) - betSoFar, pot)

    (s.copy(players = ps, pot = pot, currentBet = currentBet, streetBets = bets, history = history, phase = GamePhase.Betting(sPhase.street, pending)), events)
  }

  private def startStreet(s: GameState, street: Street, comm: List[Card], deck: Deck): (GameState, List[GameEvent]) = {
    val pending = orderedSeatsFrom(s.eligibleSeats, s.eligibleSeats(0), includeStart = true, s.players)
    val ns = s.copy(phase = GamePhase.Betting(street, pending), community = comm, deck = deck, currentBet = 0, streetBets = Map.empty)
    (ns, List(GameEvent.StreetStarted(street, comm)))
  }

  private def awardPot(s: GameState): (GameState, List[GameEvent]) = {
    val remaining = activePlayers(s.players, s.eligibleSeats)
    var ps = s.players
    val idx = remaining.head
    ps = ps.updated(idx, ps(idx).addChips(s.pot))

    val (busted, finalPs) = handleBusted(s.players, ps)
    val nextDealer = nextActiveIndex(finalPs, s.dealerIdx % finalPs.size, finalPs.size)
    val ns = s.copy(players = finalPs, phase = GamePhase.AwaitingStart, roundNum = s.roundNum + 1, dealerIdx = nextDealer)

    (ns, GameEvent.PotWon(List((ps(idx).name, s.pot)), None) :: busted)
  }

  private def handleBusted(before: List[Player], after: List[Player]): (List[GameEvent], List[Player]) = {
    val events = before.zip(after).collect {
      case (p, c) if p.chips > 0 && c.chips <= 0 => GameEvent.PlayerBusted(c.name)
    }
    (events, after)
  }

  private def isConnected(p: Player, connected: Set[String]): Boolean = p.playerType == PlayerType.Computer || connected.contains(p.name)
  private def activePlayers(ps: List[Player], eligible: List[Int]): List[Int] = eligible.filter(i => !ps(i).folded)

  private def orderedSeatsFrom(eligible: List[Int], startSeat: Int, includeStart: Boolean, ps: List[Player]): List[Int] = {
    val startIdx = eligible.indexOf(startSeat)
    if (startIdx < 0 || eligible.isEmpty) return Nil
    val offset = if (includeStart) 0 else 1
    (offset until (eligible.size + offset)).map(k => eligible((startIdx + k) % eligible.size)).filter(i => !ps(i).folded && ps(i).chips > 0).toList
  }

  @tailrec
  private def nextActiveIndex(players: List[Player], current: Int, n: Int): Int = {
    val next = (current + 1) % n
    if (players(next).chips > 0) next else nextActiveIndex(players, next, n)
  }
}