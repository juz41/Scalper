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

    def send(name: String, msg: String): Unit =
      clientMap.get(name).foreach(cid => if (session.clients.containsKey(cid)) io.send(cid, msg))

    def ask(name: String, prompt: String): String =
      clientMap.get(name).filter(session.clients.containsKey).map(io.ask(_, prompt)).getOrElse("f")

    // Process events emitted by the game engine
    def handleEvents(events: List[GameEvent]): Unit = {
      events.foreach {
        case GameEvent.RoundStarted(rnd, dealer, chips) =>
          io.broadcast(s"\n─── Round $rnd ─── Dealer: $dealer")
          chips.foreach { case (name, amount, isConn) =>
            val status = if (!isConn) " (Disconnected)" else ""
            io.broadcast(s" $name: $amount chips$status")
          }
          io.broadcast("[Start in 5 seconds...\nLast chance for /bot]")
          Thread.sleep(5000)

        case GameEvent.BlindsPosted(sb, sbAmt, bb, bbAmt) =>
          io.broadcast(s"\nBlinds: $sb posts SB $sbAmt, $bb posts BB $bbAmt | Pot: ${sbAmt + bbAmt}")

        case GameEvent.HoleCardsDealt(secrets) =>
          secrets.foreach { case (pName, hand) =>
            send(pName, s" [SECRET] Your hole cards: $hand")
          }

        case GameEvent.StreetStarted(street, comm) =>
          io.broadcast(s"\n── $street ── Community: ${comm.mkString(" ")}")

        case GameEvent.PlayerActed(pName, action, added, pot) =>
          action match {
            case Action.Fold => io.broadcast(s"$pName folds.")
            case Action.Call if added == 0 => io.broadcast(s"$pName checks.")
            case Action.Call => io.broadcast(s"$pName calls $added. Pot: $pot")
            case Action.Raise(amt) => io.broadcast(s"$pName raises to $amt (added $added). Pot: $pot")
          }

        case GameEvent.ShowdownRevealed(comm, hands) =>
          io.broadcast("\n══ SHOWDOWN ══")
          io.broadcast(s"Community: ${comm.mkString(" ")}")
          hands.foreach { case (name, hand) => io.broadcast(s" $name: $hand") }
          io.broadcast("")

        case GameEvent.PotWon(winners, rankOpt) =>
          val totalPot = winners.map(_._2).sum
          if (winners.size > 1) {
            io.broadcast(s"Tie! Pot of $totalPot split among ${winners.map(_._1).mkString(", ")}")
          } else {
            val byText = rankOpt.map(r => s" with $r!").getOrElse(" (all others folded)!")
            io.broadcast(s"${winners.head._1} wins the pot of $totalPot$byText")
          }

        case GameEvent.PlayerBusted(pName) =>
          io.broadcast(s"\n$pName has no chips left and is removed from the table.")

        case GameEvent.PlayerDisconnected(pName) =>
          io.broadcast(s"[$pName] lost connection. Automatic fold.")

        case GameEvent.GameFinished(reason) =>
          io.broadcast(s"\n$reason")

        case GameEvent.ActionRequested(pName, toCall, pot, chips) =>
          val player = gameState.players.find(_.name == pName).get

          val actionToApply = if (player.playerType == PlayerType.Human) {
            val callWord = if (toCall == 0) "check (c)" else s"call $toCall (c)"
            val raiseOpt = if (chips > toCall) s" | raise <chips to add> (r 80)" else ""
            val prompt = s"[Your move] chips:$chips pot:$pot to-call:$toCall | fold(f) $callWord$raiseOpt"

            var validAction: Option[Action] = None
            while (validAction.isEmpty) {
              val raw = ask(pName, prompt).toLowerCase
              if (raw.startsWith("/")) {
                send(pName, "Commands during turn ignored by engine (enter f, c or r) have been processed by lobby.")
              } else {
                validAction = raw match {
                  case "f" => Some(Action.Fold)
                  case "c" => Some(Action.Call)
                  case s if s.startsWith("r ") =>
                    val extra = s.drop(2).toIntOption.getOrElse(0)
                    val betSoFar = gameState.streetBets.getOrElse(gameState.players.indexOf(player), 0)
                    if (extra > 0 && extra <= chips && (extra > toCall || extra == chips)) Some(Action.Raise(extra + betSoFar))
                    else {
                      val minAdd = (toCall + 1).min(chips)
                      send(pName, s"Invalid raise. Chips to add must be >= $minAdd and <= $chips (or all-in).")
                      None
                    }
                  case _ =>
                    send(pName, "Unknown action. Use: f / c / r <amount>")
                    None
                }
              }
            }
            validAction.get
          } else {
            val currentStreet = gameState.phase match {
              case GamePhase.Betting(st, _) => st
              case _                        => Street.PreFlop
            }

            val activeSeats = gameState.eligibleSeats.filter(i => !gameState.players(i).folded)
            val seatIdx = gameState.players.indexWhere(_.name == pName)

            val opponentsChips = activeSeats
              .filterNot(_ == seatIdx)
              .map(i => gameState.players(i).name -> gameState.players(i).chips)

            val opponentsPositions = activeSeats
              .filterNot(_ == seatIdx)
              .map(i => gameState.players(i).name -> gameState.seatPositions.getOrElse(i, "Unknown"))
              .toMap

            val ctx = GameContext(
              street = currentStreet,
              community = gameState.community,
              holeCards = player.hand,
              pot = pot,
              toCall = toCall,
              availableChips = chips,
              currentBet = gameState.currentBet,
              numActivePlayers = activeSeats.size,
              opponentStacks = opponentsChips,
              position = gameState.seatPositions.getOrElse(seatIdx, "Unknown"),
              opponentPositions = opponentsPositions,
              history = gameState.history
            )

            io.broadcast(s"${player.name} (${ctx.position}) thinks...")
            Thread.sleep(1500)
            ComputerAI.decideAction(ctx)
          }

          // Apply validated action to the game engine
          GameEngine.processPlayerAction(gameState, pName, actionToApply) match {
            case Right((sAfterAction, actionEvents)) =>
              gameState = sAfterAction
              handleEvents(actionEvents)
            case Left(err) =>
              send(pName, err)
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