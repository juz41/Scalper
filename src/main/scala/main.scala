package App

import Game.*

object Main extends App {

  val players = List(
    new Player("Alice",   1000),
    new Player("Bob",     1000),
    new Player("Charlie", 1000),
    new Player("Diana",   1000)
  )

  def resetChips(): Unit = players.foreach(_.chips = 1000)

  def run(config: GameConfig, hands: Int = 3): Unit = {
    println(s"\n┌─────────────────────────────────────────┐")
    println(s"│  ${config.toString.padTo(39, ' ')}│")
    println(s"└─────────────────────────────────────────┘")
    val game = new PokerGame(players, config)
    (1 to hands).foreach(_ => game.playHand())
    game.printStandings()
    resetChips()
  }

  // Standard presets
  run(GameConfig.TexasHoldem)
  run(GameConfig.Omaha)

  // Fully custom config — e.g. high-stakes Omaha on a 60-card deck
  run(GameConfig(
    name       = "High-Stakes Omaha",
    deckConfig = DeckConfig(numRanks = 10, numSuits = 6, highRank = 10),
    evaluator  = OmahaStrategy,
    smallBlind = 50,
    bigBlind   = 100
  ))
}
