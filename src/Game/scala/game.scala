package Game

import scala.util.Random

/**
 * Runs a poker game driven entirely by a [[GameConfig]].
 * No variant switching exists inside this class — all rules come from the config.
 */
class PokerGame(val players: List[Player], val config: GameConfig) {
  require(players.length >= 2, "Need at least 2 players")

  private var deck:           Deck       = new Deck(config.deckConfig)
  private var communityCards: List[Card] = Nil
  private val pot:            Pot        = new Pot
  private var dealerIndex:    Int        = 0
  private var handNumber:     Int        = 0

  def playHand(): Unit = {
    handNumber += 1
    println(s"\n═══════════════ Hand #$handNumber | $config ═══════════════")

    val active = players.filter(_.chips > 0)
    if (active.length < 2) {
      println("Not enough players with chips to continue.")
      return
    }

    setup(active)
    postBlinds(active)
    dealHoleCards(active)

    if (bettingRound("Pre-Flop", active, firstAction = true)) {
      dealFlop()
      if (bettingRound("Flop", active)) {
        dealTurn()
        if (bettingRound("Turn", active)) {
          dealRiver()
          bettingRound("River", active)
        }
      }
    }

    showdown(active)
    printChips()
    advanceDealer(active)
  }

  private def setup(active: List[Player]): Unit = {
    deck = new Deck(config.deckConfig)
    communityCards = Nil
    pot.reset()
    active.foreach(_.resetForNewRound())
  }

  private def postBlinds(active: List[Player]): Unit = {
    val sb       = active((dealerIndex + 1) % active.length)
    val bb       = active((dealerIndex + 2) % active.length)
    val sbAmount = sb.placeBet(config.smallBlind)
    val bbAmount = bb.placeBet(config.bigBlind)
    pot.addBet(sb.name, sbAmount)
    pot.addBet(bb.name, bbAmount)
    println(s"  Blinds → ${sb.name} posts $sbAmount (SB), ${bb.name} posts $bbAmount (BB)")
  }

  private def dealHoleCards(active: List[Player]): Unit = {
    (0 until config.evaluator.holeCardCount).foreach { _ =>
      active.foreach(p => p.receiveCard(deck.deal()))
    }
    active.foreach(p => println(s"  ${p.name} hole cards: ${p.holeCards.mkString(" ")}"))
  }

  private def dealFlop(): Unit = {
    communityCards = (0 until 3).map(_ => deck.deal()).toList
    println(s"\n  ── Flop: ${communityCards.mkString(" ")} ──")
  }

  private def dealTurn(): Unit = {
    communityCards = communityCards :+ deck.deal()
    println(s"\n  ── Turn: ${communityCards.last}  (board: ${communityCards.mkString(" ")}) ──")
  }

  private def dealRiver(): Unit = {
    communityCards = communityCards :+ deck.deal()
    println(s"\n  ── River: ${communityCards.last}  (board: ${communityCards.mkString(" ")}) ──")
  }

  private def bettingRound(street: String, allPlayers: List[Player], firstAction: Boolean = false): Boolean = {
    println(s"\n  [$street] Pot: ${pot.total}")

    if (allPlayers.count(!_.isFolded) == 1) return false

    var betLevel    = allPlayers.map(_.currentBet).max
    val startIdx    = if (firstAction) (dealerIndex + 3) % allPlayers.length
                      else             (dealerIndex + 1) % allPlayers.length
    val order       = allPlayers.indices.toList.map(i => allPlayers((startIdx + i) % allPlayers.length))
    var actionsLeft = order.count(_.canAct)

    (order.to(LazyList) #::: LazyList.continually(order).flatten)
      .filter(_.canAct)
      .take(allPlayers.length * 4)
      .takeWhile(_ => allPlayers.count(!_.isFolded) > 1 && actionsLeft > 0)
      .foreach { player =>
        val toCall = betLevel - player.currentBet
        decideAction(player, toCall) match {
          case Fold =>
            player.isFolded = true
            println(s"    ${player.name} folds")

          case Check =>
            println(s"    ${player.name} checks")

          case Call(amount) =>
            val paid = player.placeBet(amount)
            pot.addBet(player.name, paid)
            println(s"    ${player.name} calls $paid")

          case Raise(amount) =>
            val paid = player.placeBet(amount)
            pot.addBet(player.name, paid)
            betLevel    = player.currentBet
            actionsLeft = allPlayers.count(p => !p.isFolded && !p.isAllIn && p != player)
            println(s"    ${player.name} raises to ${player.currentBet}")

          case AllIn(amount) =>
            val paid = player.placeBet(amount)
            pot.addBet(player.name, paid)
            player.isAllIn = true
            if (player.currentBet > betLevel) betLevel = player.currentBet
            println(s"    ${player.name} goes all-in for $paid")
        }
        actionsLeft -= 1
      }

    allPlayers.count(!_.isFolded) > 1
  }

  private def decideAction(player: Player, toCall: Int): PlayerAction = {
    if (player.chips == 0)      return Check
    if (toCall >= player.chips) return AllIn(player.chips)
    val rand = Random.nextDouble()
    if (toCall == 0) {
      if (rand < 0.7) Check else Raise(math.min(config.bigBlind * 2, player.chips))
    } else {
      if (rand < 0.15)      Fold
      else if (rand < 0.75) Call(toCall)
      else                  Raise(math.min(toCall + config.bigBlind * 2, player.chips))
    }
  }

  private def showdown(allPlayers: List[Player]): Unit = {
    val survivors = allPlayers.filter(!_.isFolded)
    println(s"\n  ══ Showdown ══")

    if (survivors.length == 1) {
      val winner = survivors.head
      println(s"  ${winner.name} wins ${pot.total} chips (everyone folded)")
      winner.chips += pot.total
      return
    }

    val evaluated = survivors.map { p =>
      val hand = config.evaluator.bestHand(p.holeCards, communityCards, config.deckConfig)
      println(s"  ${p.name}: ${p.holeCards.mkString(" ")} → $hand")
      (p, hand)
    }

    val best    = evaluated.map(_._2).max
    val winners = evaluated.filter(_._2 == best).map(_._1)
    val share   = pot.total / winners.length

    winners.foreach { w =>
      w.chips += share
      println(s"  🏆 ${w.name} wins $share chips with $best!")
    }
    val remainder = pot.total % winners.length
    if (remainder > 0) winners.head.chips += remainder
  }

  private def printChips(): Unit = {
    println(s"\n  Chip counts:")
    players.foreach(p => println(s"    $p"))
  }

  private def advanceDealer(active: List[Player]): Unit =
    dealerIndex = (dealerIndex + 1) % active.length

  def standings: List[(String, Int)] =
    players.map(p => p.name -> p.chips).sortBy(-_._2)

  def printStandings(): Unit = {
    println("\n══ Final Standings ══")
    standings.zipWithIndex.foreach { case ((name, chips), i) =>
      println(s"  ${i + 1}. $name — $chips chips")
    }
  }
}
