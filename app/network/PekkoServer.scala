package network

import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.ws.{Message, TextMessage}
import org.apache.pekko.http.scaladsl.server.Directives._
import org.apache.pekko.stream.OverflowStrategy
import org.apache.pekko.stream.scaladsl.{Flow, Sink, Source}

import java.util.concurrent.{ArrayBlockingQueue, ConcurrentHashMap}
import scala.concurrent.ExecutionContext
import scala.util.Random

// Specjalna implementacja GameIO przypisana tylko do konkretnej sesji
class SessionIO(val sessionCode: String) extends GameIO {
  override def broadcast(msg: String): Unit = PekkoServer.broadcastToSession(sessionCode, msg)
  override def send(clientId: String, msg: String): Unit = PekkoServer.sendTo(clientId, msg)
  override def ask(clientId: String, promptText: String): String = {
    PekkoServer.prompt(clientId, promptText)
    val q = new ArrayBlockingQueue[String](1)
    PekkoServer.replyQueues.put(clientId, q)
    val reply = q.take() // Czeka na wpis (odblokowywane automatycznie przy wyjściu z serwera)
    PekkoServer.replyQueues.remove(clientId)
    reply
  }
}

object PekkoServer {
  implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "PokerServer")
  implicit val ec: ExecutionContext = system.executionContext

  val clientQueues = new ConcurrentHashMap[String, org.apache.pekko.stream.scaladsl.SourceQueueWithComplete[Message]]()
  val replyQueues = new ConcurrentHashMap[String, ArrayBlockingQueue[String]]()

  val clientNames = new ConcurrentHashMap[String, String]()
  val clientSessions = new ConcurrentHashMap[String, String]() // clientId -> sessionCode
  val sessions = new ConcurrentHashMap[String, GameSession]()

  sealed trait ClientState
  case object Naming extends ClientState
  case object Menu extends ClientState
  case object EnteringCode extends ClientState
  case class InSession(code: String) extends ClientState

  val clientStates = new ConcurrentHashMap[String, ClientState]()

  def sendTo(clientId: String, msg: String): Unit = {
    Option(clientQueues.get(clientId)).foreach(_.offer(TextMessage(s"MSG:$msg")))
  }

  def prompt(clientId: String, p: String): Unit = {
    Option(clientQueues.get(clientId)).foreach(_.offer(TextMessage(s"PROMPT:$p")))
  }

  def broadcastToSession(sessionCode: String, msg: String): Unit = {
    Option(sessions.get(sessionCode)).foreach { session =>
      session.clients.keySet().forEach(cid => sendTo(cid, msg))
    }
  }

  def start(port: Int): Unit = {
    val route = path("poker") {
      handleWebSocketMessages(websocketFlow())
    }
    Http().newServerAt("0.0.0.0", port).bind(route)
    println(s"Serwer wystartował na porcie $port. Gotowy na hostowanie sesji.")
  }

  def handleDisconnect(clientId: String): Unit = {
    clientQueues.remove(clientId)
    clientStates.remove(clientId)
    Option(clientSessions.get(clientId)).foreach { code =>
      Option(sessions.get(code)).foreach { session =>
        val name = clientNames.getOrDefault(clientId, "Gracz")
        session.removeClient(clientId)
        broadcastToSession(code, s"[$name opuścił sesję]")
        if (session.isDead) {
          sessions.remove(code)
          println(s"Sesja $code zamknięta (brak ludzkich graczy).")
        }
      }
    }
    clientSessions.remove(clientId)
    clientNames.remove(clientId)

    // Jeśli gra akurat czeka na ruch tego gracza, wstrzykujemy 'f' (fold), by odblokować wątek
    Option(replyQueues.get(clientId)).foreach(_.put("f"))
  }

  def websocketFlow(): Flow[Message, Message, Any] = {
    val clientId = java.util.UUID.randomUUID().toString

    val in = Sink.foreach[Message] {
      case TextMessage.Strict(text) if text.startsWith("INPUT:") =>
        val input = text.drop(6).trim
        Option(replyQueues.get(clientId)) match {
          case Some(q) => q.put(input) // Gra aktualnie prosi tego klienta o akcję
          case None => handleAsyncCommand(clientId, input) // Asynchroniczne wiadomości
        }
      case _ =>
    }

    val out = Source.queue[Message](100, OverflowStrategy.dropTail)
      .mapMaterializedValue { queue =>
        clientQueues.put(clientId, queue)
        clientStates.put(clientId, Naming)
        queue.offer(TextMessage("PROMPT:Podaj swoje imię (wpisz i wciśnij Enter):"))
        queue
      }

    Flow.fromSinkAndSourceCoupled(in, out).watchTermination() { (_, f) =>
      f.onComplete(_ => handleDisconnect(clientId))
    }
  }

  def handleAsyncCommand(clientId: String, input: String): Unit = {
    val state = clientStates.getOrDefault(clientId, Naming)
    state match {
      case Naming =>
        val name = if (input.isEmpty) s"Gracz-${Random.nextInt(1000)}" else input
        clientNames.put(clientId, name)
        clientStates.put(clientId, Menu)
        sendMenu(clientId)

      case Menu =>
        if (input == "1") {
          val code = Random.alphanumeric.take(4).mkString.toUpperCase
          val session = new GameSession(code)
          sessions.put(code, session)
          joinSession(clientId, code, session)
        } else if (input == "2") {
          clientStates.put(clientId, EnteringCode)
          prompt(clientId, "Podaj 4-znakowy kod sesji:")
        } else {
          sendMenu(clientId)
        }

      case EnteringCode =>
        val code = input.toUpperCase
        val session = sessions.get(code)
        if (session != null) {
          joinSession(clientId, code, session)
        } else {
          sendTo(clientId, "Błędny kod sesji.")
          clientStates.put(clientId, Menu)
          sendMenu(clientId)
        }

      case InSession(code) =>
        val session = sessions.get(code)
        if (session == null) return

        input match {
          case "/start" =>
            if (session.isPlaying.compareAndSet(false, true)) {
              broadcastToSession(code, "\n*** ROZPOCZYNAMY GRĘ ***")
              new Thread(() => {
                NetworkGame.start(code, session)
              }).start()
            } else {
              sendTo(clientId, "Gra już wystartowała! Jeśli dopiero dołączyłeś, poczekaj na nową rundę.")
            }
          case "/bot" =>
            session.addBot()
            broadcastToSession(code, "Dodano bota. Dołączy w następnym rozdaniu.")
          case "/wyjdz" =>
            handleDisconnect(clientId) // symulacja odłączenia, by posprzątać
            clientStates.put(clientId, Menu)
            sendMenu(clientId)
          case _ =>
            val name = clientNames.get(clientId)
            broadcastToSession(code, s"[Chat] $name: $input")
        }
    }
  }

  def sendMenu(clientId: String): Unit = {
    sendTo(clientId, "\n--- MENU GŁÓWNE LOBBY ---")
    sendTo(clientId, "1. Stwórz nową grę")
    sendTo(clientId, "2. Dołącz do istniejącej gry")
    prompt(clientId, "Wybierz (1/2):")
  }

  def joinSession(clientId: String, code: String, session: GameSession): Unit = {
    clientStates.put(clientId, InSession(code))
    clientSessions.put(clientId, code)
    val name = clientNames.get(clientId)
    session.addClient(clientId, name)
    broadcastToSession(code, s"*** $name dołączył do pokoju ***")
    sendTo(clientId, s"--- LOBBY SESJI: $code ---")
    sendTo(clientId, "Asynchroniczne komendy: /start (start gry), /bot (dodaj AI), /wyjdz (opuść sesję). Cokolwiek innego działa jak Chat.")
  }
}