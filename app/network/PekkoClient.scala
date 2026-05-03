package network

import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import org.apache.pekko.stream.OverflowStrategy
import org.apache.pekko.stream.scaladsl.{Flow, Keep, Sink, Source}

import scala.concurrent.Future
import scala.io.StdIn

object PekkoClient {
  def start(host: String, port: Int, name: String): Unit = {
    implicit val system = ActorSystem(Behaviors.empty, "PokerClient")
    implicit val ec = system.executionContext

    val req = WebSocketRequest(s"ws://$host:$port/poker")

    val (queue, source) = Source.queue[Message](100, OverflowStrategy.dropTail).preMaterialize()

    val sink = Sink.foreach[Message] {
      case TextMessage.Strict(msg) =>
        if (msg.startsWith("MSG:")) {
          println(msg.drop(4))
        } else if (msg.startsWith("PROMPT:")) {
          print(msg.drop(7) + " ")
          // Pytamy gracza asynchronicznie, by nie zablokować strumienia WebSockets
          Future {
            val input = StdIn.readLine()
            queue.offer(TextMessage(s"REPLY:$input"))
          }
        }
      case _ =>
    }

    val flow = Flow.fromSinkAndSource(sink, source).watchTermination()(Keep.right)
    val (upgradeResponse, closed) = Http().singleWebSocketRequest(req, flow)

    // Od razu po podłączeniu przesyłamy swoje imię do serwera
    queue.offer(TextMessage(s"NAME:$name"))

    closed.onComplete(_ => {
      println("\nRozłączono z serwerem.")
      system.terminate()
    })
  }
}