package network

import engine._
import model._
import scala.collection.concurrent.TrieMap

object NetworkGame {

  private val StartingChips = 1000

  private def retrieveBots(session: GameSession): List[Player] = {
    val count = session.pendingBots.getAndSet(0)
    (1 to count).map(i => Player(s"Bot ${System.currentTimeMillis() % 100}_$i", StartingChips, Hand.empty, PlayerType.Computer)).toList
  }

  def start(sessionCode: String, session: GameSession): Unit = {
    val io = new SessionIO(sessionCode)
    val clientMap = TrieMap.empty[String, String]

    var initialPlayers = List.empty[Player]
    session.clients.forEach { (cid, name) =>
      initialPlayers :+= Player(name, StartingChips, Hand.empty, PlayerType.Human)
      clientMap.put(name, cid)
    }

    var gameState = GameState(players = initialPlayers ++ retrieveBots(session))

    def connectedSet(): Set[String] = clientMap.keys.filter(n => session.clients.containsKey(clientMap(n))).toSet

//    def send(name: String, msg: String): Unit =
//      clientMap.get(name).foreach(cid => if (session.clients.containsKey(cid)) io.send(cid, msg))
//
//    def ask(name: String, prompt: String): String =
//      clientMap.get(name).filter(session.clients.containsKey).map(io.ask(_, prompt)).getOrElse("f")

    // Process events emitted by the game engine
    def handleEvents(events: List[GameEvent]): Unit = {
      events.foreach {
        case GameEvent.RoundStarted(rnd, dealer, chips) =>
          io.broadcast(ServerToClient.GameLog(s"─── Round $rnd ─── Dealer: $dealer"))
          chips.foreach { case (name, amount, isConn) =>
            io.broadcast(ServerToClient.SyncPlayer(name, amount, folded = false, bet = 0, active = false))
          }
          io.broadcast(ServerToClient.GameLog("[Start in 5 seconds...\nLast chance for /bot]"))
          Thread.sleep(5000)

        case GameEvent.BlindsPosted(sb, sbAmt, bb, bbAmt) =>
          io.broadcast(ServerToClient.GameLog(s"Blinds: $sb posts $sbAmt, $bb posts $bbAmt"))
          io.broadcast(ServerToClient.SyncState(sbAmt + bbAmt, "Pre-Flop", Nil, bbAmt))
          io.broadcast(ServerToClient.SyncPlayer(sb, -1, folded = false, bet = sbAmt, active = false))
          io.broadcast(ServerToClient.SyncPlayer(bb, -1, folded = false, bet = bbAmt, active = false))

        case GameEvent.HoleCardsDealt(secrets) =>
          secrets.foreach { case (pName, hand) =>
            io.send(clientMap(pName), ServerToClient.HoleCards(hand.cards))
          }

        case GameEvent.StreetStarted(street, comm) =>
          io.broadcast(ServerToClient.SyncState(gameState.pot, street.toString, comm, 0))

        case GameEvent.PlayerActed(pName, action, added, pot) =>
          action match {
            case Action.Fold =>
              io.broadcast(ServerToClient.GameLog(s"$pName folds."))
              io.broadcast(ServerToClient.SyncPlayer(pName, -1, folded = true, bet = 0, active = false))
            case Action.Call if added == 0 =>
              io.broadcast(ServerToClient.GameLog(s"$pName checks."))
            case Action.Call =>
              io.broadcast(ServerToClient.GameLog(s"$pName calls $added."))
              io.broadcast(ServerToClient.SyncPlayer(pName, -1, folded = false, bet = gameState.streetBets.getOrElse(gameState.players.indexWhere(_.name == pName), 0), active = false))
            case Action.Raise(amt) =>
              io.broadcast(ServerToClient.GameLog(s"$pName raises to $amt (added $added)."))
              io.broadcast(ServerToClient.SyncPlayer(pName, -1, folded = false, bet = amt, active = false))
          }

        case GameEvent.ShowdownRevealed(comm, hands) =>
          io.broadcast(ServerToClient.GameLog("══ SHOWDOWN ══"))
          hands.foreach { case (name, hand) =>
            io.broadcast(ServerToClient.ShowdownCards(name, hand.cards))
          }

        case GameEvent.PotWon(winners, rankOpt) =>
          val totalPot = winners.map(_._2).sum
          if (winners.size > 1) {
            io.broadcast(ServerToClient.GameLog(s"Tie! Pot of $totalPot split among ${winners.map(_._1).mkString(", ")}"))
          } else {
            val byText = rankOpt.map(r => s" with $r!").getOrElse(" (all others folded)!")
            io.broadcast(ServerToClient.GameLog(s"${winners.head._1} wins the pot of $totalPot$byText"))
          }

        case GameEvent.PlayerBusted(pName) =>
          io.broadcast(ServerToClient.GameLog(s"$pName has no chips left."))

        case GameEvent.PlayerDisconnected(pName) =>
          io.broadcast(ServerToClient.PlayerLeft(pName))

        case GameEvent.GameFinished(reason) =>
          io.broadcast(ServerToClient.GameLog(s"\n$reason"))
          io.broadcast(ServerToClient.LobbyState(inLobby = true))

        case GameEvent.ActionRequested(pName, toCall, pot, chips) =>
          val player = gameState.players.find(_.name == pName).get

          io.broadcast(ServerToClient.SyncPlayer(pName, -1, folded = false, bet = -1, active = true))

          if (player.playerType == PlayerType.Human) {
            val actionMsg = io.ask(clientMap(pName), ServerToClient.PromptAction(toCall, (toCall + 1).min(chips), chips))
            actionMsg match {
              case ClientToServer.PlayerAct(act) => GameEngine.processPlayerAction(gameState, pName, act) match {
                case Right((sAfter, evs)) =>
                  gameState = sAfter
                  handleEvents(evs)
                case Left(_) => // Wpadnie tutaj jeżeli user wstrzyknie niedozwoloną akcję
              }
              case _ => // Przypadek wyjścia/rozłączenia
            }
          } else {
            // Logika bota zostaje bez zmian (oblicza i woła GameEngine)
            val currentStreet = gameState.phase match {
              case GamePhase.Betting(st, _) => st
              case _                        => Street.PreFlop
            }
            val activeSeats = gameState.eligibleSeats.filter(i => !gameState.players(i).folded)
            val seatIdx = gameState.players.indexWhere(_.name == pName)
            val opponentsChips = activeSeats.filterNot(_ == seatIdx).map(i => gameState.players(i).name -> gameState.players(i).chips)
            val opponentsPositions = activeSeats.filterNot(_ == seatIdx).map(i => gameState.players(i).name -> gameState.seatPositions.getOrElse(i, "Unknown")).toMap

            val ctx = GameContext(currentStreet, gameState.community, player.hand, pot, toCall, chips, gameState.currentBet, activeSeats.size, opponentsChips, gameState.seatPositions.getOrElse(seatIdx, "Unknown"), opponentsPositions, gameState.history)

            Thread.sleep(1500)
            val act = ComputerAI.decideAction(ctx)
            GameEngine.processPlayerAction(gameState, pName, act) match {
              case Right((sAfter, evs)) => gameState = sAfter; handleEvents(evs)
              case Left(_) =>
            }
          }
      }
    }

    // Game loop using game engine
    while (!session.isDead && gameState.phase != GamePhase.GameOver) {

      if (gameState.phase == GamePhase.AwaitingStart) {
        val bots = retrieveBots(session)
        var humans = List.empty[Player]
        while (!session.pendingPlayers.isEmpty) {
          val (cid, name) = session.pendingPlayers.poll()
          humans :+= Player(name, StartingChips, Hand.empty, PlayerType.Human)
          clientMap.put(name, cid)
        }
        gameState = gameState.copy(players = gameState.players ++ bots ++ humans)
      }

      val (newState, events) = GameEngine.advance(gameState, connectedSet())
      gameState = newState

      // Handle events from the engine step
      handleEvents(events)
    }

    session.isPlaying.set(false)
  }
}