package network

import scala.io.StdIn

object MultiplayerApp {
  def main(args: Array[String]): Unit = {
    println("=== SCALPER MULTIPLAYER ===")

    args.headOption match {
      case Some("--server") =>
        val port    = argValue(args, "--port").flatMap(_.toIntOption).getOrElse(8080)
        val players = argValue(args, "--players").flatMap(_.toIntOption).getOrElse(2)
        PekkoServer.start(port, players)

      case Some("--client") =>
        val host = argValue(args, "--address").getOrElse {
          println("Błąd: --client wymaga --address <ip>")
          sys.exit(1)
        }
        val port = argValue(args, "--port").flatMap(_.toIntOption).getOrElse {
          println("Błąd: --client wymaga --port <port>")
          sys.exit(1)
        }
        val name = argValue(args, "--name").getOrElse {
          print("Podaj swoje imię w grze: ")
          val n = StdIn.readLine().trim
          if (n.isEmpty) "Gracz" else n
        }
        PekkoClient.start(host, port, name)

      case _ =>
        println("Użycie:")
        println("  --server [--port 8080] [--players 2]")
        println("  --client --address <ip> --port <port> [--name <imię>]")
        sys.exit(1)
    }
  }

  private def argValue(args: Array[String], flag: String): Option[String] = {
    val idx = args.indexOf(flag)
    if (idx >= 0 && idx + 1 < args.length) Some(args(idx + 1)) else None
  }
}