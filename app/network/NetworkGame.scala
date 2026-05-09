package network

import engine.{GameEngine, EngineIO}
import model.{Player, PlayerType, Hand}
import scala.collection.concurrent.TrieMap

object NetworkGame {

  private val StartingChips = 1000

  private def retrievePendingBots(session: GameSession): List[Player] = {
    val initialBotsCount = session.pendingBots.getAndSet(0)
    (1 to initialBotsCount).map { i =>
      Player(s"Bot ${System.currentTimeMillis() % 10000}_$i", StartingChips, Hand.empty, PlayerType.Computer)
    }.toList
  }

  def start(sessionCode: String, session: GameSession): Unit = {
    val io = new SessionIO(sessionCode)

    // Dynamic map linking player names to their connection IDs (clientIds).
    val clientMap = TrieMap.empty[String, String]

    var initialPlayers = List.empty[Player]
    session.clients.forEach { (cid, name) =>
      initialPlayers = initialPlayers :+ Player(name, StartingChips, Hand.empty, PlayerType.Human)
      clientMap.put(name, cid)
    }

    // Retrieve bots added in the lobby before the start
    val initialBots = retrievePendingBots(session)

    // EngineIO implementation controlling the WebSocket flow
    val engineIO = new EngineIO {
      override def broadcast(msg: String): Unit = io.broadcast(msg)

      override def send(playerName: String, msg: String): Unit = {
        clientMap.get(playerName).foreach { cid =>
          if (session.clients.containsKey(cid)) io.send(cid, msg)
        }
      }

      override def ask(playerName: String, prompt: String): String = {
        clientMap.get(playerName) match {
          case Some(cid) if session.clients.containsKey(cid) =>
            io.ask(cid, prompt)
          case _ =>
            "f" // Automatic fold if the connection is lost during a decision request
        }
      }

      override def isConnected(playerName: String): Boolean = {
        clientMap.get(playerName).exists(cid => session.clients.containsKey(cid))
      }

      override def isGameActive(): Boolean = !session.isDead

      override def fetchNewPlayers(): List[Player] = {
        if (session.isDead) return Nil

        // Adding new bots waiting in the queue from the lobby
        val bots = retrievePendingBots(session)

        // Adding late-joining human players from the lobby
        var humans = List.empty[Player]
        while (!session.pendingPlayers.isEmpty) {
          val (cid, name) = session.pendingPlayers.poll()
          humans = humans :+ Player(name, StartingChips, Hand.empty, PlayerType.Human)
          clientMap.put(name, cid)
        }

        bots ++ humans
      }

      override def onGameEnded(): Unit = {
        session.isPlaying.set(false)
      }
    }

    // Initialize and start the shared game engine
    val engine = new GameEngine(engineIO, initialPlayers ++ initialBots)
    engine.start()
  }
}