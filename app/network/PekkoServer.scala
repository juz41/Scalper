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

object PekkoServer extends GameIO {
  //implicit val system = ActorSystem(Behaviors.empty, "PokerServer")
  implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "PokerServer")
  implicit val ec: ExecutionContext = system.executionContext

  val clientQueues = new ConcurrentHashMap[String, org.apache.pekko.stream.scaladsl.SourceQueueWithComplete[Message]]()
  val replyQueues = new ConcurrentHashMap[String, ArrayBlockingQueue[String]]()
  val clientNames = new ConcurrentHashMap[String, String]()

  var connectedClients = List.empty[String]

  override def broadcast(msg: String): Unit = {
    println(s"BROADCAST: $msg") // Podgląd na serwerze
    clientQueues.values().forEach(_.offer(TextMessage(s"MSG:$msg")))
  }

  override def send(clientId: String, msg: String): Unit = {
    Option(clientQueues.get(clientId)).foreach(_.offer(TextMessage(s"MSG:$msg")))
  }

  override def ask(clientId: String, prompt: String): String = {
    Option(clientQueues.get(clientId)).foreach(_.offer(TextMessage(s"PROMPT:$prompt")))
    val q = new ArrayBlockingQueue[String](1)
    replyQueues.put(clientId, q)
    val reply = q.take() // Blokuje wątek gry (nie serwera webowego!) dopóki klient nie odpowie
    replyQueues.remove(clientId)
    reply
  }

  def start(port: Int, expectedPlayers: Int): Unit = {
    val route = path("poker") {
      handleWebSocketMessages(websocketFlow(expectedPlayers))
    }
    Http().newServerAt("0.0.0.0", port).bind(route)
    println(s"Serwer wystartował na porcie $port. Oczekiwanie na $expectedPlayers graczy...")
  }

  def websocketFlow(expectedPlayers: Int): Flow[Message, Message, Any] = {
    val clientId = java.util.UUID.randomUUID().toString

    val in = Sink.foreach[Message] {
      case TextMessage.Strict(text) if text.startsWith("REPLY:") =>
        Option(replyQueues.get(clientId)).foreach(_.put(text.drop(6)))

      case TextMessage.Strict(text) if text.startsWith("NAME:") =>
        val name = text.drop(5)
        clientNames.put(clientId, name)

        if (clientNames.size() == expectedPlayers) {
          println("Wszyscy gracze podłączeni. Uruchamiam grę...")
          val clientsMap = connectedClients.map(cid => cid -> clientNames.get(cid)).toMap

          // Uruchamiamy grę w nowym wątku, by pętla nie blokowała strumieni Pekko
          new Thread(() => {
            NetworkGame.start(this, clientsMap)
          }).start()
        }
      case _ =>
    }

    val out = Source.queue[Message](100, OverflowStrategy.dropTail)
      .mapMaterializedValue { queue =>
        clientQueues.put(clientId, queue)
        connectedClients = connectedClients :+ clientId
        println(s"Połączono klienta. Razem: ${connectedClients.size}/$expectedPlayers")
        queue
      }

    Flow.fromSinkAndSourceCoupled(in, out)
  }
}