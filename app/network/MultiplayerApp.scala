package network

object MultiplayerApp {
  def main(args: Array[String]): Unit = {
    println("=== SCALPER ===")

    args.headOption match {
      case Some("--server") =>
        val port    = argValue(args, "--port").flatMap(_.toIntOption).getOrElse(2137)
        PekkoServer.start(port)

      case Some("--client") =>
        val host = argValue(args, "--address").getOrElse {
          println("Error: --client needs --address <ip>")
          sys.exit(1)
        }
        val port = argValue(args, "--port").flatMap(_.toIntOption).getOrElse {
          println("Error: --client needs --port <port>")
          sys.exit(1)
        }

        PekkoClient.start(host, port)

      case _ =>
        println("Usage:")
        println("  --server [--port 2137]")
        println("  --client --address <ip> --port <port>")
        sys.exit(1)
    }
  }

  private def argValue(args: Array[String], flag: String): Option[String] = {
    val idx = args.indexOf(flag)
    if (idx >= 0 && idx + 1 < args.length) Some(args(idx + 1)) else None
  }
}