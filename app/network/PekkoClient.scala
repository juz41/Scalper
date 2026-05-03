package network

import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import org.apache.pekko.stream.OverflowStrategy
import org.apache.pekko.stream.scaladsl.{Flow, Keep, Sink, Source}

import scala.io.StdIn

object PekkoClient {
  def start(host: String, port: Int): Unit = {
    implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "PokerClient")
    import system.executionContext

    val req = WebSocketRequest(s"ws://$host:$port/poker")
    val (queue, source) = Source.queue[Message](100, OverflowStrategy.dropTail).preMaterialize()

    val sink = Sink.foreach[Message] {
      case TextMessage.Strict(msg) =>
        if (msg.startsWith("MSG:")) {
          println(msg.drop(4))
        } else if (msg.startsWith("PROMPT:")) {
          print(msg.drop(7) + " ")
        }
      case _ =>
    }

    val flow = Flow.fromSinkAndSource(sink, source).watchTermination()(Keep.right)
    val (upgradeResponse, closed) = Http().singleWebSocketRequest(req, flow)

    // Wątek odpowiedzialny za ciągłe słuchanie klawiatury
    val inputThread = new Thread(() => {
      var continue = true
      while (continue) {
        val input = StdIn.readLine()
        if (input == null) {
          continue = false
        } else {
          queue.offer(TextMessage(s"INPUT:$input"))
        }
      }
    })
    inputThread.setDaemon(true)
    inputThread.start()

    closed.onComplete(_ => {
      println("\nRozłączono z serwerem.")
      system.terminate()
      sys.exit(0)
    })
  }
}