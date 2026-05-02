package App

import Game.*
import org.scalajs.dom
import org.scalajs.dom.document

object Main {
  def main(args: Array[String]): Unit = {
    // Odpalamy logikę na start
    val config = GameConfig.TexasHoldem
    val deck = new Deck(config.deckConfig)

    val p1 = new Player("Vibe Coder", 1000)
    val p2 = new Player("Bot AI", 1000)

    // Symulacja rozdania
    p1.receiveCard(deck.deal())
    p1.receiveCard(deck.deal())
    p2.receiveCard(deck.deal())
    p2.receiveCard(deck.deal())

    val flop1 = deck.deal()
    val flop2 = deck.deal()
    val flop3 = deck.deal()

    val app = document.getElementById("app")

    // Wstrzykujemy UI z powrotem do HTML
    app.innerHTML = s"""
      <div class="table">
        <div class="pot">
          Pula: ${config.smallBlind + config.bigBlind} $$<br/>
          <span style="font-size: 14px; color: #ddd">${config.name}</span>
        </div>

        <div class="cards board">
          <div class="card">${formatCard(flop1)}</div>
          <div class="card">${formatCard(flop2)}</div>
          <div class="card">${formatCard(flop3)}</div>
        </div>

        <div class="players">
          <div class="player">
            <h3>${p2.name} (${p2.chips} $$)</h3>
            <div class="cards">
              <div class="card card-back"></div>
              <div class="card card-back"></div>
            </div>
          </div>

          <div class="player">
            <h3>${p1.name} (${p1.chips} $$)</h3>
            <div class="cards">
              <div class="card">${formatCard(p1.holeCards(0))}</div>
              <div class="card">${formatCard(p1.holeCards(1))}</div>
            </div>
          </div>
        </div>

        <div class="actions">
          <button class="btn btn-fold" onclick="location.reload()">Fold</button>
          <button class="btn btn-call" onclick="location.reload()">Call</button>
          <button class="btn btn-raise" onclick="location.reload()">Raise</button>
        </div>
      </div>
    """
  }

  // Funkcja mapująca Twoje wartości int z config.scala na ładne symbole
  def formatCard(c: Card): String = {
    val suits = Array("", "♠", "♥", "♦", "♣") // 1=Pik, 2=Kier, 3=Karo, 4=Trefl
    val ranks = Array("", "A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")

    val suitSymbol = if (c.suit <= 4) suits(c.suit) else c.suit.toString
    val rankStr = if (c.rank <= 13) ranks(c.rank) else c.rank.toString

    // Kolorujemy kiery i kara na czerwono
    val color = if (c.suit == 2 || c.suit == 3) "#d32f2f" else "black"

    s"""<span style="color: $color">$rankStr $suitSymbol</span>"""
  }
}