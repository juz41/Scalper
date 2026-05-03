package network

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

class GameSession(val code: String) {
  // clientId -> imię gracza
  val clients = new ConcurrentHashMap[String, String]()

  // Kolejka graczy oraz botów oczekujących na wejście do gry (w następnym rozdaniu)
  val pendingBots = new AtomicInteger(0)
  val pendingPlayers = new ConcurrentLinkedQueue[(String, String)]()

  val isPlaying = new AtomicBoolean(false)
  @volatile var isDead = false

  def addClient(clientId: String, name: String): Unit = {
    clients.put(clientId, name)
    if (isPlaying.get()) {
      pendingPlayers.add((clientId, name))
    }
  }

  def removeClient(clientId: String): Unit = {
    clients.remove(clientId)
    if (clients.isEmpty) {
      isDead = true
    }
  }

  def addBot(): Unit = {
    pendingBots.incrementAndGet()
  }
}