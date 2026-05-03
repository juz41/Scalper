package network

import engine._
import model._
import logic.HandEvaluator
import scala.annotation.tailrec

object NetworkGame {

  private val StartingChips = 1000
  private val SmallBlind    = 10
  private val BigBlind      = 20

  def start(sessionCode: String, session: GameSession): Unit = {
    val io = new SessionIO(sessionCode)

    var clientMap = Map.empty[String, String]
    var initialPlayers = List.empty[Player]

    session.clients.forEach { (cid, name) =>
      initialPlayers = initialPlayers :+ Player(name, StartingChips, Hand.empty, PlayerType.Human)
      clientMap = clientMap + (name -> cid)
    }

    // Boty startowe (jeśli ktoś kliknął /bot w poczekalni)
    val initialBotsCount = session.pendingBots.getAndSet(0)
    val initialBots = (1 to initialBotsCount).map { i =>
      Player(s"Bot ${System.currentTimeMillis() % 10000}_$i", StartingChips, Hand.empty, PlayerType.Computer)
    }.toList

    gameLoop(io, session, clientMap, initialPlayers ++ initialBots, dealerIndex = 0, round = 1)
  }

  @tailrec
  private def gameLoop(
                        io: GameIO,
                        session: GameSession,
                        cmap: Map[String, String],
                        currentPlayers: List[Player],
                        dealerIndex: Int,
                        round: Int
                      ): Unit = {
    if (session.isDead) return

    // Dodanie nowych botów oczekujących w kolejce
    val newBotsCount = session.pendingBots.getAndSet(0)
    val bots = (1 to newBotsCount).map { i =>
      Player(s"Bot ${System.currentTimeMillis() % 10000}_$i", StartingChips, Hand.empty, PlayerType.Computer)
    }.toList

    // Dodanie spóźnionych ludzkich graczy
    var humans = List.empty[Player]
    var updatedCmap = cmap
    while (!session.pendingPlayers.isEmpty) {
      val (cid, name) = session.pendingPlayers.poll()
      humans = humans :+ Player(name, StartingChips, Hand.empty, PlayerType.Human)
      updatedCmap = updatedCmap + (name -> cid)
    }

    val players = currentPlayers ++ bots ++ humans
    val active = players.filter(p => p.chips > 0 && isPlayerConnected(p, session, updatedCmap))

    if (active.size < 2) {
      if (active.size == 1) io.broadcast(s"\n${active.head.name} wygrywa grę (brak aktywnych przeciwników)!")
      else io.broadcast("\nBrak graczy zdolnych do dalszej gry.")
      session.isPlaying.set(false)
      return
    }

    io.broadcast(s"\n─── Runda $round ───  Dealer: ${players(dealerIndex % players.size).name}")
    players.foreach { p =>
      val status = if(!isPlayerConnected(p, session, updatedCmap)) " (Odłączony)" else ""
      io.broadcast(s"  ${p.name}: ${p.chips} żetonów$status")
    }

    io.broadcast("[Za 3 sekundy gramy... Ostatnia szansa na /bot!]")
    Thread.sleep(3000)

    val updatedPlayers = playRound(io, session, updatedCmap, players, dealerIndex % players.size)
    if (session.isDead) return

    val nextDealer = nextActiveIndex(updatedPlayers, session, updatedCmap, dealerIndex, updatedPlayers.size)
    gameLoop(io, session, updatedCmap, updatedPlayers, nextDealer, round + 1)
  }

  private def isPlayerConnected(p: Player, session: GameSession, cmap: Map[String, String]): Boolean = {
    if (p.playerType == PlayerType.Computer) true
    else cmap.get(p.name).exists(cid => session.clients.containsKey(cid))
  }

  private def nextActiveIndex(players: List[Player], session: GameSession, cmap: Map[String, String], current: Int, n: Int): Int = {
    val next = (current + 1) % n
    if (players(next).chips > 0 && isPlayerConnected(players(next), session, cmap)) next
    else nextActiveIndex(players, session, cmap, next, n)
  }

  private def playRound(io: GameIO, session: GameSession, cmap: Map[String, String], allPlayers: List[Player], dealerIdx: Int): List[Player] = {
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
    io.broadcast(s"\nBlinds: ${freshPlayers(sbSeat).name} wplaca SB $SmallBlind, ${freshPlayers(bbSeat).name} wplaca BB $BigBlind  |  Pot: $pot")

    var deck = Deck.standard.shuffle()
    players = players.zipWithIndex.map { case (p, i) =>
      if (eligibleSeats.contains(i)) {
        val (cards, rest) = deck.deal(2)
        deck = rest
        p.withHand(Hand(cards))
      } else p
    }

    showHoleCards(io, session, cmap, players, eligibleSeats)

    val utg = (eligibleSeats.indexOf(bbSeat) + 1) % eligibleSeats.size
    var history = List.empty[ActionRecord]
    val seatPositions = calculatePositions(eligibleSeats, dealerIdx)

    // Pre-flop
    val (ps1, pot1, history1, ended1) = bettingRound(io, session, cmap, players, eligibleSeats, pot, BigBlind, initialBets.clone(), utg, Nil, Street.PreFlop, history, seatPositions)
    players = ps1; pot = pot1; history = history1
    if (ended1 || activePlayers(players, eligibleSeats).size == 1) return awardPot(io, players, pot, eligibleSeats)

    // Flop
    val (flop, d2) = deck.deal(3); deck = d2
    val community1 = flop
    io.broadcast(s"\n── Flop ──  Stol: ${showCards(community1)}")
    showHoleCards(io, session, cmap, players, eligibleSeats)

    val (ps2, pot2, history2, ended2) = bettingRound(io, session, cmap, players, eligibleSeats, pot, 0, Array.fill(n)(0), 0, community1, Street.Flop, history, seatPositions)
    players = ps2; pot = pot2; history = history2
    if (ended2 || activePlayers(players, eligibleSeats).size == 1) return awardPot(io, players, pot, eligibleSeats)

    // Turn
    val (turn, d3) = deck.deal(1); deck = d3
    val community2 = community1 ++ turn
    io.broadcast(s"\n── Turn ──  Stol: ${showCards(community2)}")
    showHoleCards(io, session, cmap, players, eligibleSeats)

    val (ps3, pot3, history3, ended3) = bettingRound(io, session, cmap, players, eligibleSeats, pot, 0, Array.fill(n)(0), 0, community2, Street.Turn, history, seatPositions)
    players = ps3; pot = pot3; history = history3
    if (ended3 || activePlayers(players, eligibleSeats).size == 1) return awardPot(io, players, pot, eligibleSeats)

    // River
    val (river, _) = deck.deal(1)
    val community3 = community2 ++ river
    io.broadcast(s"\n── River ── Stol: ${showCards(community3)}")
    showHoleCards(io, session, cmap, players, eligibleSeats)

    val (ps4, pot4, _, ended4) = bettingRound(io, session, cmap, players, eligibleSeats, pot, 0, Array.fill(n)(0), 0, community3, Street.River, history, seatPositions)
    players = ps4; pot = pot4
    if (ended4 || activePlayers(players, eligibleSeats).size == 1) return awardPot(io, players, pot, eligibleSeats)

    showdown(io, players, eligibleSeats, pot4, community3)
  }

  private def activePlayers(players: List[Player], eligibleSeats: List[Int]): List[Int] =
    eligibleSeats.filter(i => !players(i).folded)

  private def bettingRound(io: GameIO, session: GameSession, cmap: Map[String, String], players: List[Player], eligibleSeats: List[Int], pot: Int, currentBet: Int, streetBets: Array[Int], firstActIdx: Int, community: List[model.Card], street: Street, history: List[ActionRecord], seatPositions: Map[Int, String]): (List[Player], Int, List[ActionRecord], Boolean) = {
    io.broadcast(s"\n── Licytacja: $street ──")
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
      if (session.isDead) return (ps, potV, historyV, true)

      val seatI = eligibleSeats(actionIdx % numSeats)
      val player = ps(seatI)

      if (!player.folded && player.chips > 0) {
        val toCall = (currentBetV - bets(seatI)).max(0).min(player.chips)

        val action = player.playerType match {
          case PlayerType.Human =>
            cmap.get(player.name) match {
              case Some(cid) if session.clients.containsKey(cid) =>
                humanAction(io, cid, player, potV, toCall, community)
              case _ =>
                io.broadcast(s"[$player.name] utracił połączenie. Automatyczny Fold.")
                Action.Fold
            }
          case PlayerType.Computer =>
            val ctx = GameContext(street, community, player.hand, potV, toCall, player.chips, currentBetV, activePlayers(ps, eligibleSeats).size, activePlayers(ps, eligibleSeats).filterNot(i => ps(i).name == player.name).map(i => ps(i).name -> ps(i).chips), seatPositions.getOrElse(seatI, "Unknown"), activePlayers(ps, eligibleSeats).filterNot(i => ps(i).name == player.name).map(i => ps(i).name -> seatPositions.getOrElse(i, "Unknown")).toMap, historyV)
            val a = ComputerAI.decideAction(ctx)
            io.broadcast(s"${player.name} (${ctx.position}) myśli...  -> $a")
            Thread.sleep(1500)
            a
        }

        val resolvedAction = action match {
          case Action.Fold =>
            ps = ps.updated(seatI, player.fold)
            if(player.playerType == PlayerType.Human) io.broadcast(s"${player.name} pasuje (fold).")
            Action.Fold
          case Action.Call =>
            if (toCall == 0) {
              io.broadcast(s"${player.name} czeka (check).")
            } else {
              val actual = toCall.min(player.chips)
              ps = ps.updated(seatI, player.removeChips(actual))
              bets(seatI) += actual
              potV += actual
              io.broadcast(s"${player.name} sprawdza (call) $actual.  Pot: $potV")
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
            io.broadcast(s"${player.name} podbija do $currentBetV.  Pot: $potV")
            Action.Raise(currentBetV)
        }

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

  private def humanAction(io: GameIO, clientId: String, player: Player, pot: Int, toCall: Int, community: List[model.Card]): Action = {
    val callWord = if (toCall == 0) "check (c)" else s"call $toCall (c)"
    val raiseOpt = if (player.chips > toCall) "  |  raise <ile_dodac> (r 80)" else ""
    val prompt = s"[Twoj ruch] zetonow:${player.chips}  pula:$pot  do_sprawdzenia:$toCall  |  fold(f)  $callWord$raiseOpt"

    @tailrec def readAction(): Action = {
      val rawInput = io.ask(clientId, prompt).toLowerCase
      // Użytkownik mógł wpisać komendę /bot albo /wyjdz w trakcie proszenia o ruch
      if (rawInput.startsWith("/")) {
        io.send(clientId, "Komendy w trakcie ruchu ignorowane przez silnik (wpisz f, c lub r). Zostały przetworzone przez Lobby.")
        readAction()
      } else {
        rawInput match {
          case "f" => Action.Fold
          case "c" => Action.Call
          case s if s.startsWith("r ") =>
            val extra = s.drop(2).toIntOption.getOrElse(0)
            if (extra > toCall && extra <= player.chips) Action.Raise(extra)
            else {
              io.send(clientId, s"Bledne podbicie. Dodaj wartosc od ${toCall + 1} do ${player.chips}")
              readAction()
            }
          case _ =>
            io.send(clientId, "Nieznana akcja. Uzyj: f / c / r <ilosc>")
            readAction()
        }
      }
    }
    readAction()
  }

  private def showdown(io: GameIO, players: List[Player], eligibleSeats: List[Int], pot: Int, community: List[model.Card]): List[Player] = {
    io.broadcast("\n══ SHOWDOWN ══")
    io.broadcast(s"Karty na stole: ${showCards(community)}")
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
      io.broadcast(s"Remis! Pula $pot podzielona pomiedzy ${winners.map(_._1).mkString(", ")} (po $share)")
      winners.foldLeft(players) { case (ps, (name, _)) =>
        val idx = eligibleSeats.find(i => players(i).name == name).getOrElse(-1)
        if (idx >= 0) ps.updated(idx, ps(idx).addChips(share)) else ps
      }
    } else {
      val (winnerName, evalHand) = winners.head
      io.broadcast(s"$winnerName wygrywa pule $pot z układem ${evalHand.rank.name}!")
      val winIdx = eligibleSeats.find(i => players(i).name == winnerName).getOrElse(-1)
      if (winIdx >= 0) players.updated(winIdx, players(winIdx).addChips(pot)) else players
    }
  }

  private def awardPot(io: GameIO, players: List[Player], pot: Int, eligibleSeats: List[Int]): List[Player] = {
    val remaining = activePlayers(players, eligibleSeats)
    if (remaining.size == 1) {
      val idx = remaining.head
      val winner = players(idx)
      io.broadcast(s"\n${winner.name} zgarnia pule $pot (reszta spasowala)!")
      players.updated(idx, winner.addChips(pot))
    } else players
  }

  private def showCards(cards: List[model.Card]): String = cards.map(_.toString).mkString("  ")

  private def showHoleCards(io: GameIO, session: GameSession, cmap: Map[String, String], players: List[Player], eligibleSeats: List[Int]): Unit = {
    eligibleSeats.foreach { i =>
      val p = players(i)
      if (p.playerType == PlayerType.Human && !p.folded) {
        cmap.get(p.name).foreach { cid =>
          if(session.clients.containsKey(cid)) io.send(cid, s"  [SEKRET] Twoje karty: ${p.hand}")
        }
      }
    }
  }

  private def calculatePositions(eligibleSeats: List[Int], dealerIdx: Int): Map[Int, String] = {
    val n = eligibleSeats.size
    val dealerPos = eligibleSeats.indexOf(dealerIdx) match {
      case -1 => val after = eligibleSeats.indexWhere(_ > dealerIdx); if (after >= 0) after else 0
      case idx => idx
    }
    if (n == 2) {
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