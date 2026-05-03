package network

object MultiplayerApp {
  def main(args: Array[String]): Unit = {
    println("=== SCALPER MULTIPLAYER ===")
    println("1. Uruchom jako SERWER (Host)")
    println("2. Uruchom jako KLIENT (Gracz)")
    print("Wybierz opcję (1/2): ")

    StdIn.readLine().trim match {
      case "1" =>
        print("Podaj port nasłuchiwania (domyślnie 8080): ")
        val port = StdIn.readLine().trim.toIntOption.getOrElse(8080)

        print("Ilu ludzkich graczy ma zagrać? (domyślnie 2): ")
        val players = StdIn.readLine().trim.toIntOption.getOrElse(2)

        PekkoServer.start(port, players)

      case "2" =>
        print("Podaj adres IP serwera (domyślnie localhost): ")
        val hostInput = StdIn.readLine().trim
        val host = if (hostInput.isEmpty) "localhost" else hostInput

        print("Podaj port serwera (domyślnie 8080): ")
        val port = StdIn.readLine().trim.toIntOption.getOrElse(8080)

        print("Podaj swoje imię w grze: ")
        val nameInput = StdIn.readLine().trim
        val name = if (nameInput.isEmpty) "Gracz" else nameInput

        PekkoClient.start(host, port, name)

      case _ =>
        println("Niepoprawny wybór. Koniec.")
    }
  }
}