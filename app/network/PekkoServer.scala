package network

import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.ws.{Message, TextMessage}
import org.apache.pekko.http.scaladsl.server.Directives._
import org.apache.pekko.http.scaladsl.settings.ServerSettings
import org.apache.pekko.stream.OverflowStrategy
import org.apache.pekko.stream.scaladsl.{Flow, Sink, Source}

import java.util.concurrent.{ArrayBlockingQueue, ConcurrentHashMap}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Random

class SessionIO(val sessionCode: String) extends GameIO {
  override def broadcast(msg: String): Unit = PekkoServer.broadcastToSession(sessionCode, msg)
  override def send(clientId: String, msg: String): Unit = PekkoServer.sendTo(clientId, msg)
  override def ask(clientId: String, promptText: String): String = {
    PekkoServer.prompt(clientId, promptText)
    val q = new ArrayBlockingQueue[String](1)
    PekkoServer.replyQueues.put(clientId, q)
    val reply = q.take()
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
  val clientSessions = new ConcurrentHashMap[String, String]()
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

    val defaultSettings = ServerSettings(system)
    val settings = defaultSettings.withTimeouts(
      defaultSettings.timeouts.withIdleTimeout(Duration.Inf)
    )

    Http().newServerAt("0.0.0.0", port)
      .withSettings(settings)
      .bind(route)
    println(s"Server started at port $port.")
  }

  def handleDisconnect(clientId: String): Unit = {
    clientQueues.remove(clientId)
    clientStates.remove(clientId)
    Option(clientSessions.get(clientId)).foreach { code =>
      Option(sessions.get(code)).foreach { session =>
        val name = clientNames.getOrDefault(clientId, "Player")
        session.removeClient(clientId)
        broadcastToSession(code, s"[$name disconnected]")
        if (session.isDead) {
          sessions.remove(code)
          println(s"Session $code closed (no human players).")
        }
      }
    }
    clientSessions.remove(clientId)
    clientNames.remove(clientId)

    // If game waits for player move, inject 'f' (fold) to unlock thread
    Option(replyQueues.get(clientId)).foreach(_.put("f"))
  }

  def websocketFlow(): Flow[Message, Message, Any] = {
    val clientId = java.util.UUID.randomUUID().toString

    val keepAliveMessage = TextMessage.Strict("")

    val in = Sink.foreach[Message] {
      case TextMessage.Strict(text) if text.startsWith("INPUT:") =>
        val input = text.drop(6).trim
        Option(replyQueues.get(clientId)) match {
          case Some(q) => q.put(input)
          case None => handleAsyncCommand(clientId, input)
        }
      case _ =>
    }

    val out = Source.queue[Message](100, OverflowStrategy.dropTail)
      .mapMaterializedValue { queue =>
        clientQueues.put(clientId, queue)
        clientStates.put(clientId, Naming)
        queue.offer(TextMessage("PROMPT:Enter name:"))
        queue
      }

    Flow.fromSinkAndSourceCoupled(in, out)
      .keepAlive(30.seconds, () => TextMessage.Strict(keepAliveMessage.getStrictText))
      .watchTermination() { (_, f) =>
        f.onComplete(_ => handleDisconnect(clientId))
    }
  }

  def handleAsyncCommand(clientId: String, input: String): Unit = {
    val state = clientStates.getOrDefault(clientId, Naming)
    state match {
      case Naming =>
        val name = if (input.isEmpty) s"Player-${Random.nextInt(1000)}" else input
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
          prompt(clientId, "Enter 4-character session code:")
        } else {
          sendMenu(clientId)
        }

      case EnteringCode =>
        val code = input.toUpperCase
        val session = sessions.get(code)
        if (session != null) {
          joinSession(clientId, code, session)
        } else {
          sendTo(clientId, "Invalid session code.")
          clientStates.put(clientId, Menu)
          sendMenu(clientId)
        }

      case InSession(code) =>
        val session = sessions.get(code)
        if (session == null) return

        input match {
          case "/start" =>
            if (session.isPlaying.compareAndSet(false, true)) {
              broadcastToSession(code, "\n*** START GAME ***")
              new Thread(() => {
                NetworkGame.start(code, session)
              }).start()
            } else {
              sendTo(clientId, "Game already started! If you just joined wait for a new round.")
            }
          case "/bot" =>
            session.addBot()
            broadcastToSession(code, "Added bot.")
          case "/leave" =>
            handleDisconnect(clientId)
            clientStates.put(clientId, Menu)
            sendMenu(clientId)
          case _ =>
            val name = clientNames.get(clientId)
            broadcastToSession(code, s"[Chat] $name: $input")
        }
    }
  }

  def sendMenu(clientId: String): Unit = {
    sendTo(clientId, "\n--- LOBBY MAIN MENU ---")
    sendTo(clientId, "1. Create new game")
    sendTo(clientId, "2. Join existing game")
    prompt(clientId, "Choose (1/2):")
  }

  def joinSession(clientId: String, code: String, session: GameSession): Unit = {
    clientStates.put(clientId, InSession(code))
    clientSessions.put(clientId, code)
    val name = clientNames.get(clientId)
    session.addClient(clientId, name)
    broadcastToSession(code, s"*** $name joined room ***")
    sendTo(clientId, s"--- SESSION'S CODE: $code ---")
    sendTo(clientId, "Commands: /start (start), /bot (add bot), /leave (leave room).")
  }
}