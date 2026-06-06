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
import upickle.default._

class SessionIO(val sessionCode: String) extends GameIO {
  override def broadcast(msg: ServerToClient): Unit = PekkoServer.broadcastToSession(sessionCode, msg)
  override def send(clientId: String, msg: ServerToClient): Unit = PekkoServer.sendTo(clientId, msg)
  override def ask(clientId: String, promptInfo: ServerToClient): ClientToServer = {
    val q = new ArrayBlockingQueue[ClientToServer](1)
    PekkoServer.replyQueues.put(clientId, q)
    PekkoServer.sendTo(clientId, promptInfo)
    val reply = q.take()
    PekkoServer.replyQueues.remove(clientId, q)
    reply
  }
}

object PekkoServer {
  implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "PokerServer")
  implicit val ec: ExecutionContext = system.executionContext

  val clientQueues = new ConcurrentHashMap[String, org.apache.pekko.stream.scaladsl.SourceQueueWithComplete[Message]]()
  val replyQueues = new ConcurrentHashMap[String, ArrayBlockingQueue[ClientToServer]]()

  val clientNames = new ConcurrentHashMap[String, String]()
  val clientSessions = new ConcurrentHashMap[String, String]()
  val sessions = new ConcurrentHashMap[String, GameSession]()

  sealed trait ClientState
  case object Naming extends ClientState
  case object Menu extends ClientState
  case object EnteringCode extends ClientState
  case class InSession(code: String) extends ClientState

  val clientStates = new ConcurrentHashMap[String, ClientState]()

  def sendTo(clientId: String, msg: ServerToClient): Unit = {
    val jsonStr = write(msg)
    Option(clientQueues.get(clientId)).foreach(_.offer(TextMessage(jsonStr)))
  }

  def broadcastToSession(sessionCode: String, msg: ServerToClient): Unit = {
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
        broadcastToSession(code, ServerToClient.GameLog(s"[$name disconnected]"))
        if (session.isDead) {
          sessions.remove(code)
          println(s"Session $code closed (no human players).")
        }
      }
    }
    clientSessions.remove(clientId)
    clientNames.remove(clientId)

    Option(replyQueues.get(clientId)).foreach(_.put(ClientToServer.PlayerAct(engine.Action.Fold)))
  }

  def websocketFlow(): Flow[Message, Message, Any] = {
    val clientId = java.util.UUID.randomUUID().toString

    val keepAliveMessage = TextMessage.Strict("")

    val in = Sink.foreach[Message] {
      case TextMessage.Strict(text) =>
        try {
          val clientMsg = read[ClientToServer](text)
          Option(replyQueues.get(clientId)) match {
            case Some(q) => q.put(clientMsg)
            case None    => handleAsyncCommand(clientId, clientMsg)
          }
        } catch {
          case e: Exception =>
            println(s"[WS:$clientId] Ignoring malformed client payload: ${e.getMessage}")
        }
      case _ =>
    }

    val out = Source.queue[Message](100, OverflowStrategy.dropTail)
      .mapMaterializedValue { queue =>
        clientQueues.put(clientId, queue)
        clientStates.put(clientId, Naming)
        queue.offer(TextMessage(write(ServerToClient.PromptMenu("Enter your name:"))))
        queue
      }

    Flow.fromSinkAndSourceCoupled(in, out)
      .keepAlive(30.seconds, () => TextMessage.Strict(keepAliveMessage.getStrictText))
      .watchTermination() { (_, f) =>
        f.onComplete(_ => handleDisconnect(clientId))
      }
  }

  def handleAsyncCommand(clientId: String, msg: ClientToServer): Unit = {
    val state = clientStates.getOrDefault(clientId, Naming)

    msg match {
      case ClientToServer.ChatInput(input) =>
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
              sendTo(clientId, ServerToClient.PromptMenu("Enter 4-character session code:"))
            } else sendMenu(clientId)

          case EnteringCode =>
            val code = input.toUpperCase
            val session = sessions.get(code)
            if (session != null) joinSession(clientId, code, session)
            else {
              sendTo(clientId, ServerToClient.GameLog("Invalid session code.", false))
              clientStates.put(clientId, Menu)
              sendMenu(clientId)
            }

          case InSession(code) =>
            val session = sessions.get(code)
            if (session != null) {
              val name = clientNames.get(clientId)
              broadcastToSession(code, ServerToClient.GameLog(s"$name: $input", isChat = true))
            }
        }

      case ClientToServer.LobbyCmd(cmd) =>
        state match {
          case InSession(code) =>
            val session = sessions.get(code)
            if (session != null) {
              cmd match {
                case "/start" =>
                  if (session.isPlaying.compareAndSet(false, true)) {
                    broadcastToSession(code, ServerToClient.LobbyState(inLobby = false))
                    broadcastToSession(code, ServerToClient.GameLog("*** START GAME ***", false))
                    new Thread(() => NetworkGame.start(code, session)).start()
                  } else sendTo(clientId, ServerToClient.GameLog("Game already started!", false))
                case "/bot" =>
                  session.addBot()
                  broadcastToSession(code, ServerToClient.GameLog("Added bot.", false))
                case "/leave" =>
                  handleDisconnect(clientId)
                  clientStates.put(clientId, Menu)
                  sendMenu(clientId)
                case _ =>
              }
            }
          case _ =>
        }

      case ClientToServer.PlayerAct(_) => // Ignorowane poza głównym loopem gry
    }
  }

  def sendMenu(clientId: String): Unit = {
    sendTo(clientId, ServerToClient.GameLog("--- LOBBY MAIN MENU ---\n1. Create new game\n2. Join existing game"))
    sendTo(clientId, ServerToClient.PromptMenu("Choose (1/2):"))
  }

  def joinSession(clientId: String, code: String, session: GameSession): Unit = {
    clientStates.put(clientId, InSession(code))
    clientSessions.put(clientId, code)
    val name = clientNames.get(clientId)
    session.addClient(clientId, name)
    broadcastToSession(code, ServerToClient.PlayerJoined(name))
    sendTo(clientId, ServerToClient.SessionInfo(code))
    sendTo(clientId, ServerToClient.GameLog("Commands: /start (start), /bot (add bot), /leave (leave room)."))
  }
}