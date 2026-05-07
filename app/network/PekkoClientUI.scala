package network

import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import org.apache.pekko.stream.OverflowStrategy
import org.apache.pekko.stream.scaladsl.{Flow, Keep, Sink, Source}

import java.awt._
import java.awt.geom._
import javax.swing._
import javax.swing.border._
import javax.swing.text._

// ═══════════════════════════════════════════════════════════════════════════════
//  PALETTE
// ═══════════════════════════════════════════════════════════════════════════════
object K {
  val BG        = new Color(13,  17,  23)
  val PANEL     = new Color(22,  28,  38)
  val BORDER    = new Color(40,  52,  66)
  val FELT      = new Color(22,  92,  52)
  val FELT_EDGE = new Color(14,  62,  34)
  val FELT_HIGH = new Color(30, 115,  65)
  val WOOD      = new Color(95,  58,  18)
  val WOOD_DARK = new Color(55,  32,   8)
  val GOLD      = new Color(212,175,  55)
  val GOLD_DIM  = new Color(140,112,  30)
  val WHITE     = new Color(235,240, 248)
  val SILVER    = new Color(155,168, 185)
  val RED       = new Color(210, 52,  52)
  val GREEN     = new Color( 48,170,  72)
  val BLUE      = new Color( 52,122, 215)
  val YELLOW    = new Color(218,192,  48)
  val ACT_GL    = new Color( 48,170,  72,  55)
  val ACT_BD    = new Color( 48,170,  72)
  val CARD_BK   = new Color( 28, 50, 130)
  val INPUT_BG  = new Color( 26, 34,  46)
  val PROMPT_BG = new Color( 18, 44,  28)

  val F_BODY  = new Font("Segoe UI",   Font.PLAIN, 13)
  val F_BOLD  = new Font("Segoe UI",   Font.BOLD,  13)
  val F_SMALL = new Font("Segoe UI",   Font.PLAIN, 11)
  val F_SBOLD = new Font("Segoe UI",   Font.BOLD,  11)
  val F_MONO  = new Font("Monospaced", Font.PLAIN, 11)
  val F_GEO   = new Font("Georgia",    Font.BOLD,  16)
}

// ═══════════════════════════════════════════════════════════════════════════════
//  CARD MODEL
// ═══════════════════════════════════════════════════════════════════════════════
case class Card(rank: String, suit: String) {
  val isRed: Boolean = suit == "\u2665" || suit == "\u2666"
  override def toString: String = rank + suit
}

object Card {
  // Matches e.g. "A♠", "10♥", "K♦", "2♣"
  private val RE = """([2-9TJQKA]|10)([\u2660\u2665\u2666\u2663])""".r
  def allIn(s: String): Seq[Card] =
    RE.findAllMatchIn(s).map(m => Card(m.group(1), m.group(2))).toSeq
}

// ═══════════════════════════════════════════════════════════════════════════════
//  DATA MODEL
// ═══════════════════════════════════════════════════════════════════════════════
class Seat {
  var name:     String    = ""
  var chips:    Int       = 0
  var bet:      Int       = 0
  /** Number of face-down (back) cards to render for this opponent */
  var backCards: Int      = 0
  /** Revealed face-up cards (local player, or at showdown) */
  var hand:     Seq[Card] = Nil
  var active:   Boolean   = false
  var folded:   Boolean   = false
  var occupied: Boolean   = false
  var isLocal:  Boolean   = false
  var isDealer: Boolean   = false

  def reset(): Unit = {
    folded = false; bet = 0; active = false; hand = Nil
    backCards = if (occupied && !isLocal) 2 else 0
  }
}

class State {
  val seats: Array[Seat]   = Array.fill(8)(new Seat())
  var community: Seq[Card] = Nil
  var pot:         Int     = 0
  var myChips:     Int     = 0
  var myHand:      Seq[Card] = Nil
  var round:       String  = "Lobby"
  var sessionCode: String  = ""
  var callAmount:  Int     = 0
  var minRaise:    Int     = 0
  var inLobby:     Boolean = true

  def localSeat: Option[Seat] = seats.find(_.isLocal)

  def fullReset(): Unit = {
    community = Nil; pot = 0; myHand = Nil; callAmount = 0; minRaise = 0
    seats.foreach(_.reset())
  }

  def addPlayer(name: String, local: Boolean = false): Unit =
    // Don't add duplicates
    if (seats.exists(s => s.occupied && s.name == name)) ()
    else seats.find(!_.occupied).foreach { s =>
      s.name = name; s.occupied = true; s.isLocal = local
      s.backCards = if (local) 0 else 2
    }

  def removePlayer(name: String): Unit =
    seats.find(s => s.occupied && s.name == name).foreach { s =>
      s.occupied = false; s.name = ""; s.hand = Nil; s.backCards = 0
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
//  MESSAGE PARSER
//  Understands both English (server-side) and Polish log text
// ═══════════════════════════════════════════════════════════════════════════════
object Parser {
  // Pot / chips
  val potRe      = """(?i)(?:pot|pula)\s*[:\-]\s*(\d+)""".r
  val chipsRe    = """(?i)(?:chips?|zeton[oy]?|stack)\s*[:\-]\s*(\d+)""".r
  val callRe     = """(?i)call\s*\((\d+)\)""".r
  val minRaiseRe = """(?i)min\s*[:\-]\s*(\d+)""".r

  // Cards
  val myCardRe = """(?i)(?:your\s+cards?|twoje\s+karty|hand)\s*[:\-]\s*(.+)""".r
  val tableRe  = """(?i)(?:community|board|flop|turn|river|table)\s*[:\-]\s*(.+)""".r

  // Showdown reveal – "PlayerName: A♠ K♥"  or  "[PlayerName] shows A♠ K♥"
  val showRe   = """(?i)(?:\[([^\]]+)\]|^([A-Za-z0-9 _\-]+))\s*(?:shows?|:)\s+([2-9TJQKA10]+[\u2660\u2665\u2666\u2663].*)""".r

  // Players
  val joinRe    = """(?i)\*{2,}\s*(.+?)\s+(?:joined|dołączył)""".r
  val leftRe    = """(?i)\[([^\]]+)\]\s*(?:left|opuścił|opuscil)""".r
  val sessionRe = """(?i)(?:SESSION|SESJ[AI])\s*[:\-]\s*([A-Z0-9]{4})""".r

  // Actions
  val foldRe    = """(?i)\[([^\]]+)\]\s+(?:folds?|pasuje)""".r
  val betRe     = """(?i)\[([^\]]+)\]\s+(?:bets?|raises?|podbija)[^\d]*(\d+)""".r
  val activeRe  = """(?i)\[([^\]]+)\]\s+(?:it's your turn|action|your turn)""".r
  val dealerRe  = """(?i)\[([^\]]+)\]\s+is\s+the\s+dealer""".r

  val newRoundRe = """(?i)(?:new\s+(?:round|hand)|hand\s*#|nowa\s+runda|nowe\s+rozdanie|rozdanie\s*#)""".r
  val winRe      = """(?i)(?:wins?|winner|wygry)""".r

  def process(msg: String, st: State): Unit = {
    sessionRe.findFirstMatchIn(msg).foreach(m => st.sessionCode = m.group(1))

    if (newRoundRe.findFirstIn(msg).isDefined) {
      st.fullReset(); st.round = "Pre-Flop"
    }

    potRe.findFirstMatchIn(msg)     .foreach(m => st.pot        = m.group(1).toInt)
    chipsRe.findFirstMatchIn(msg)   .foreach(m => st.myChips    = m.group(1).toInt)
    callRe.findFirstMatchIn(msg)    .foreach(m => st.callAmount = m.group(1).toInt)
    minRaiseRe.findFirstMatchIn(msg).foreach(m => st.minRaise   = m.group(1).toInt)

    // Local player's hole cards
    myCardRe.findFirstMatchIn(msg).foreach { m =>
      val c = Card.allIn(m.group(1))
      if (c.nonEmpty) {
        st.myHand = c
        st.localSeat.foreach { s => s.hand = c; s.backCards = 0 }
      }
    }

    // Community / board cards
    tableRe.findFirstMatchIn(msg).foreach { m =>
      val c = Card.allIn(m.group(1))
      if (c.nonEmpty) {
        st.community = c
        val lo = msg.toLowerCase
        if (lo.contains("flop"))  st.round = "Flop"
        if (lo.contains("turn"))  st.round = "Turn"
        if (lo.contains("river")) st.round = "River"
      }
    }

    // Showdown card reveal for opponents
    showRe.findAllMatchIn(msg).foreach { m =>
      val name  = Option(m.group(1)).orElse(Option(m.group(2))).getOrElse("").trim
      val cards = Card.allIn(m.group(3))
      if (cards.nonEmpty && name.nonEmpty) {
        st.seats.find(s => s.occupied && s.name.equalsIgnoreCase(name)).foreach { s =>
          s.hand = cards; s.backCards = 0
        }
      }
    }

    joinRe.findFirstMatchIn(msg).foreach(m => st.addPlayer(m.group(1).trim))
    leftRe.findFirstMatchIn(msg).foreach(m => st.removePlayer(m.group(1).trim))

    foldRe.findFirstMatchIn(msg).foreach { m =>
      st.seats.find(_.name == m.group(1)).foreach { s =>
        s.folded = true; s.active = false; s.backCards = 0; s.hand = Nil
      }
    }

    betRe.findFirstMatchIn(msg).foreach { m =>
      st.seats.find(_.name == m.group(1)).foreach { s =>
        s.bet = m.group(2).toInt; s.active = false
      }
    }

    activeRe.findFirstMatchIn(msg).foreach { m =>
      st.seats.foreach(_.active = false)
      st.seats.find(_.name == m.group(1)).foreach(_.active = true)
    }

    dealerRe.findFirstMatchIn(msg).foreach { m =>
      st.seats.foreach(_.isDealer = false)
      st.seats.find(_.name == m.group(1)).foreach(_.isDealer = true)
    }

    // Lobby / game state transitions
    val lo = msg.toLowerCase
    if (lo.contains("lobby") || lo.contains("joined the room")) st.inLobby = true
    if (lo.contains("starting") || lo.contains("rozpoczynamy") ||
      newRoundRe.findFirstIn(msg).isDefined)                  st.inLobby = false
    if (winRe.findFirstIn(msg).isDefined)                        st.round   = "Showdown"
  }

  def isActionPrompt(p: String): Boolean = {
    val lo = p.toLowerCase
    lo.contains("fold") || lo.contains("call") || lo.contains("raise") || lo.contains("check") ||
      lo.contains("action") || lo.contains("akcj")
  }
  def isMenuPrompt(p: String): Boolean =
    p.contains("1") && p.contains("2") &&
      (p.toLowerCase.contains("choose") || p.toLowerCase.contains("select") ||
        p.toLowerCase.contains("wybierz") || p.toLowerCase.contains("menu"))
  def isNamePrompt(p: String): Boolean = {
    val lo = p.toLowerCase; lo.contains("name") || lo.contains("imi")
  }
  def isCodePrompt(p: String): Boolean = {
    val lo = p.toLowerCase; lo.contains("code") || lo.contains("kod")
  }
}

// ═══════════════════════════════════════════════════════════════════════════════
//  CARD RENDERER
//  Card size is fixed; every card gets rank top-left, big suit centre, rank bottom-right
// ═══════════════════════════════════════════════════════════════════════════════
object CardR {
  val W = 58; val H = 84

  private val RANK_SM  = new Font("Georgia",   Font.BOLD,  11)
  private val SUIT_SM  = new Font("Segoe UI",  Font.PLAIN, 11)
  private val SUIT_BIG = new Font("Segoe UI",  Font.PLAIN, 26)

  // Face-up card
  def drawCard(g: Graphics2D, x: Int, y: Int, c: Card): Unit = {
    val fg = if (c.isRed) new Color(195, 40, 40) else new Color(15, 15, 22)
    shadow(g, x, y)
    // Background
    g.setColor(Color.WHITE);              g.fillRoundRect(x, y, W, H, 8, 8)
    g.setColor(new Color(185, 192, 205)); g.drawRoundRect(x, y, W, H, 8, 8)
    g.setColor(fg)
    // Top-left rank + suit
    g.setFont(RANK_SM)
    g.drawString(c.rank, x + 4, y + 13)
    g.setFont(SUIT_SM)
    g.drawString(c.suit, x + 4, y + 26)
    // Centre suit (big)
    g.setFont(SUIT_BIG)
    val bfm = g.getFontMetrics
    g.drawString(c.suit, x + (W - bfm.stringWidth(c.suit)) / 2, y + H / 2 + 10)
    // Bottom-right rank (rotated 180° trick: just mirror the coordinates)
    g.setFont(RANK_SM)
    val rfm = g.getFontMetrics
    g.drawString(c.rank, x + W - rfm.stringWidth(c.rank) - 4, y + H - 5)
    g.setFont(SUIT_SM)
    val sfm = g.getFontMetrics
    g.drawString(c.suit, x + W - sfm.stringWidth(c.suit) - 4, y + H - 17)
  }

  // Face-down (back) card
  def drawBack(g: Graphics2D, x: Int, y: Int): Unit = {
    shadow(g, x, y)
    // Body
    g.setColor(K.CARD_BK); g.fillRoundRect(x, y, W, H, 8, 8)
    // Inner frame
    g.setColor(new Color(255, 255, 255, 35))
    g.drawRoundRect(x + 4, y + 4, W - 8, H - 8, 6, 6)
    // Dot pattern
    g.setColor(new Color(255, 255, 255, 18))
    for (col <- 0 until 4; row <- 0 until 5)
      g.fillOval(x + 10 + col * 11, y + 10 + row * 14, 6, 6)
    // Outer rim highlight
    g.setColor(new Color(255, 255, 255, 55))
    g.drawRoundRect(x, y, W, H, 8, 8)
  }

  // Empty card outline (community card placeholder)
  def drawEmpty(g: Graphics2D, x: Int, y: Int): Unit = {
    val s0 = g.getStroke
    g.setColor(new Color(255, 255, 255, 30))
    g.setStroke(new BasicStroke(1.4f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND,
      1f, Array(5f, 4f), 0f))
    g.drawRoundRect(x, y, W, H, 8, 8)
    g.setStroke(s0)
  }

  private def shadow(g: Graphics2D, x: Int, y: Int): Unit = {
    g.setColor(new Color(0, 0, 0, 65))
    g.fillRoundRect(x + 3, y + 5, W, H, 8, 8)
  }
}

// ═══════════════════════════════════════════════════════════════════════════════
//  TABLE PANEL  –  oval felt table + all seats
// ═══════════════════════════════════════════════════════════════════════════════
class TablePanel(val st: State) extends JPanel {
  setOpaque(true); setBackground(K.BG)

  // Seat angles (degrees, 270 = bottom = local player)
  private val SEAT_DEG = Array(270.0, 330.0, 30.0, 90.0, 150.0, 210.0, 248.0, 292.0)

  // Radius fractions for name-plate centre point
  private val RX_FRAC = 0.41
  private val RY_FRAC = 0.38

  private def seatCentre(idx: Int, w: Int, h: Int): (Int, Int) = {
    val a = SEAT_DEG(idx % SEAT_DEG.length).toRadians
    (w / 2 + ((w * RX_FRAC) * Math.cos(a)).toInt,
      h / 2 + ((h * RY_FRAC) * Math.sin(a)).toInt)
  }

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    val g2 = g.asInstanceOf[Graphics2D]
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,      RenderingHints.VALUE_ANTIALIAS_ON)
    g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g2.setRenderingHint(RenderingHints.KEY_RENDERING,         RenderingHints.VALUE_RENDER_QUALITY)
    val w = getWidth; val h = getHeight
    paintTable(g2, w, h)
    paintCommunity(g2, w, h)
    paintPot(g2, w, h)
    paintRound(g2, w, h)
    paintSeats(g2, w, h)
  }

  // ── Oval table (wood rim + felt) ──────────────────────────────────────────
  private def paintTable(g2: Graphics2D, w: Int, h: Int): Unit = {
    val cx = w / 2; val cy = h / 2
    val rx = (w * 0.44).toInt; val ry = (h * 0.42).toInt
    // drop shadow
    g2.setColor(new Color(0, 0, 0, 90))
    g2.fillOval(cx - rx - 4, cy - ry + 14, (rx + 4) * 2, (ry + 10) * 2)
    // wood rim
    g2.setPaint(new GradientPaint(0f, 0f, K.WOOD, w.toFloat, h.toFloat, K.WOOD_DARK))
    g2.fillOval(cx - rx - 16, cy - ry - 16, (rx + 16) * 2, (ry + 16) * 2)
    g2.setColor(new Color(255, 210, 130, 45))
    g2.setStroke(new BasicStroke(2.5f)); g2.drawOval(cx - rx - 16, cy - ry - 16, (rx + 16) * 2, (ry + 16) * 2)
    g2.setStroke(new BasicStroke(1))
    // felt
    g2.setPaint(new RadialGradientPaint(
      cx.toFloat, cy.toFloat, rx.toFloat,
      Array(0f, 0.60f, 1f),
      Array(K.FELT_HIGH, K.FELT, K.FELT_EDGE)
    ))
    g2.fillOval(cx - rx, cy - ry, rx * 2, ry * 2)
    // inner decorative ring
    g2.setColor(new Color(255, 255, 255, 18)); g2.setStroke(new BasicStroke(1.8f))
    g2.drawOval(cx - rx + 8, cy - ry + 8, rx * 2 - 16, ry * 2 - 16)
    g2.setStroke(new BasicStroke(1))
  }

  // ── Community cards (5 placeholders) ─────────────────────────────────────
  private def paintCommunity(g2: Graphics2D, w: Int, h: Int): Unit = {
    val cx = w / 2; val cy = h / 2
    val gap   = 8
    val total = 5 * CardR.W + 4 * gap
    val sx    = cx - total / 2
    val sy    = cy - CardR.H / 2 - 8
    for (i <- 0 until 5) {
      val x = sx + i * (CardR.W + gap)
      if (i < st.community.size) CardR.drawCard(g2, x, sy, st.community(i))
      else                        CardR.drawEmpty(g2, x, sy)
    }
  }

  // ── Pot ───────────────────────────────────────────────────────────────────
  private def paintPot(g2: Graphics2D, w: Int, h: Int): Unit = {
    if (st.pot <= 0) return
    val cx = w / 2; val cy = h / 2
    val txt = s"Pot:  ${st.pot}"
    val fnt = new Font("Georgia", Font.BOLD, 14)
    g2.setFont(fnt)
    val fm = g2.getFontMetrics
    val tx = cx - fm.stringWidth(txt) / 2
    val ty = cy + CardR.H / 2 + 32
    // chip dot
    g2.setColor(K.GOLD);     g2.fillOval(tx - 24, ty - 13, 16, 16)
    g2.setColor(K.GOLD_DIM); g2.drawOval(tx - 24, ty - 13, 16, 16)
    // text shadow
    g2.setColor(new Color(0, 0, 0, 120)); g2.drawString(txt, tx + 1, ty + 1)
    g2.setColor(K.GOLD);                  g2.drawString(txt, tx,     ty)
  }

  // ── Round label ───────────────────────────────────────────────────────────
  private def paintRound(g2: Graphics2D, w: Int, h: Int): Unit = {
    val cx = w / 2; val cy = h / 2
    val fnt = new Font("Segoe UI", Font.BOLD, 13)
    g2.setFont(fnt)
    val fm  = g2.getFontMetrics
    val tx  = cx - fm.stringWidth(st.round) / 2
    val ty  = cy - CardR.H / 2 - 24
    g2.setColor(new Color(0, 0, 0, 110)); g2.drawString(st.round, tx + 1, ty + 1)
    g2.setColor(new Color(195, 220, 200, 185)); g2.drawString(st.round, tx, ty)
  }

  // ── All seats ─────────────────────────────────────────────────────────────
  private def paintSeats(g2: Graphics2D, w: Int, h: Int): Unit = {
    val occupied = st.seats.filter(_.occupied)
    // Rotate so local player is always index 0 (bottom seat)
    val localIdx = occupied.indexWhere(_.isLocal)
    val ordered  = if (localIdx > 0) occupied.drop(localIdx) ++ occupied.take(localIdx) else occupied
    ordered.zipWithIndex.foreach { case (seat, idx) =>
      if (idx < SEAT_DEG.length) {
        val (cx, cy) = seatCentre(idx, w, h)
        paintSeat(g2, seat, cx, cy, below = (idx == 0))
      }
    }
  }

  // ── One seat: name plate + cards + dealer button + bet chip ──────────────
  private def paintSeat(g2: Graphics2D, s: Seat, cx: Int, cy: Int, below: Boolean): Unit = {
    val PW = 120; val PH = 48  // name-plate dimensions
    val px = cx - PW / 2

    // Cards are drawn ABOVE the name plate for top seats, BELOW for the bottom seat.
    // "below" means this is the local player sitting at the bottom of the screen.
    val cardGap    = 6
    val cardsTotalW = s.hand.size.max(s.backCards).max(2) * (CardR.W + cardGap) - cardGap
    val cX         = cx - cardsTotalW / 2

    if (below) {
      // Name plate sits above the cards
      val py = cy - PH / 2  // vertically centred on cy; cards go below
      paintNamePlate(g2, s, px, py, PW, PH)
      val cY = py + PH + 10
      paintCards(g2, s, cX, cY, cardGap)
      if (s.isDealer) paintDealerButton(g2, px + PW + 6, py + PH / 2 - 10)
      if (s.bet > 0)  paintBetChip(g2, s.bet, cx + PW / 2 + 16, py - 22)
    } else {
      // Name plate sits below the cards
      val py = cy - PH / 2
      val cY = py - CardR.H - 10
      paintCards(g2, s, cX, cY, cardGap)
      paintNamePlate(g2, s, px, py, PW, PH)
      if (s.isDealer) paintDealerButton(g2, px + PW + 6, py + PH / 2 - 10)
      if (s.bet > 0)  paintBetChip(g2, s.bet, cx + PW / 2 + 14, py + PH + 6)
    }
  }

  private def paintCards(g2: Graphics2D, s: Seat, cX: Int, cY: Int, gap: Int): Unit = {
    if (s.folded) return  // no cards drawn for folded players

    if (s.hand.nonEmpty) {
      // Show face-up cards (local player, or opponent at showdown)
      s.hand.zipWithIndex.foreach { case (c, i) =>
        CardR.drawCard(g2, cX + i * (CardR.W + gap), cY, c)
      }
    } else if (s.backCards > 0) {
      // Show face-down backs for active opponents
      for (i <- 0 until s.backCards)
        CardR.drawBack(g2, cX + i * (CardR.W + gap), cY)
    } else if (s.occupied) {
      // Seat occupied but no cards yet – show 2 empty placeholders
      for (i <- 0 until 2)
        CardR.drawEmpty(g2, cX + i * (CardR.W + gap), cY)
    }
  }

  private def paintNamePlate(g2: Graphics2D, s: Seat, px: Int, py: Int, pw: Int, ph: Int): Unit = {
    // Active glow
    if (s.active) {
      g2.setColor(K.ACT_GL)
      g2.fillRoundRect(px - 6, py - 6, pw + 12, ph + 12, 14, 14)
      val s0 = g2.getStroke
      g2.setColor(K.ACT_BD); g2.setStroke(new BasicStroke(2.2f))
      g2.drawRoundRect(px - 6, py - 6, pw + 12, ph + 12, 14, 14)
      g2.setStroke(s0)
    }
    // Plate background
    val bg = if (s.folded) new Color(36, 36, 48, 215) else new Color(16, 22, 32, 225)
    g2.setColor(bg); g2.fillRoundRect(px, py, pw, ph, 10, 10)
    g2.setColor(if (s.active) K.ACT_BD else K.BORDER)
    g2.drawRoundRect(px, py, pw, ph, 10, 10)

    // Name
    val nmFont = new Font("Segoe UI", Font.BOLD, 12)
    g2.setFont(nmFont); g2.setColor(if (s.folded) K.SILVER else K.WHITE)
    val nm = if (s.name.length > 12) s.name.take(12) + "\u2026" else s.name
    g2.drawString(nm, px + 8, py + 18)

    // Chip count
    val cpFont = new Font("Segoe UI", Font.PLAIN, 11)
    g2.setFont(cpFont); g2.setColor(K.GOLD)
    g2.drawString(s"${s.chips}", px + 8, py + 35)
    g2.setFont(new Font("Segoe UI", Font.PLAIN, 9)); g2.setColor(K.GOLD_DIM)
    val cpW = g2.getFontMetrics(cpFont).stringWidth(s"${s.chips}")
    g2.drawString(" chips", px + 8 + cpW, py + 35)

    // FOLD badge
    if (s.folded) {
      g2.setFont(K.F_SBOLD); g2.setColor(K.RED)
      g2.drawString("FOLD", px + pw - 40, py + 18)
    }
  }

  private def paintDealerButton(g2: Graphics2D, x: Int, y: Int): Unit = {
    g2.setColor(Color.WHITE); g2.fillOval(x, y, 20, 20)
    g2.setColor(new Color(50, 50, 50))
    g2.setFont(new Font("Segoe UI", Font.BOLD, 9))
    val fm = g2.getFontMetrics; g2.drawString("D", x + (20 - fm.stringWidth("D")) / 2, y + 14)
  }

  private def paintBetChip(g2: Graphics2D, bet: Int, x: Int, y: Int): Unit = {
    g2.setColor(K.YELLOW); g2.fillOval(x, y, 28, 28)
    g2.setColor(new Color(130, 110, 20)); g2.drawOval(x, y, 28, 28)
    val txt = if (bet >= 1000) s"${bet / 1000}k" else bet.toString
    g2.setFont(new Font("Segoe UI", Font.BOLD, 9)); g2.setColor(Color.BLACK)
    val fm = g2.getFontMetrics
    g2.drawString(txt, x + (28 - fm.stringWidth(txt)) / 2, y + 17)
  }

  def refresh(): Unit = SwingUtilities.invokeLater(() => repaint())
}

// ═══════════════════════════════════════════════════════════════════════════════
//  GAME LOG PANEL
// ═══════════════════════════════════════════════════════════════════════════════
class LogPanel extends JPanel(new BorderLayout()) {
  setBackground(K.PANEL)
  setPreferredSize(new Dimension(235, 0))
  setBorder(new MatteBorder(0, 1, 0, 0, K.BORDER))

  private val pane = new JTextPane {
    setEditable(false); setBackground(K.BG); setForeground(K.WHITE); setFont(K.F_MONO)
    setBorder(BorderFactory.createEmptyBorder(6, 8, 6, 8))
    getCaret.asInstanceOf[DefaultCaret].setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE)
  }
  private val scroll = new JScrollPane(pane) {
    setBorder(BorderFactory.createEmptyBorder())
    setBackground(K.BG); getViewport.setBackground(K.BG)
  }
  private val hdr = new JLabel("  \u2663  GAME LOG") {
    setFont(K.F_SBOLD); setForeground(K.GOLD); setOpaque(true)
    setBackground(new Color(18, 24, 34))
    setBorder(BorderFactory.createEmptyBorder(9, 8, 9, 8))
  }
  add(hdr, BorderLayout.NORTH); add(scroll, BorderLayout.CENTER)

  private val doc = pane.getStyledDocument
  private def attr(fg: Color, bold: Boolean = false): SimpleAttributeSet = {
    val a = new SimpleAttributeSet()
    StyleConstants.setForeground(a, fg); StyleConstants.setBold(a, bold)
    StyleConstants.setFontFamily(a, K.F_MONO.getFamily)
    StyleConstants.setFontSize(a, K.F_MONO.getSize); a
  }

  def addMsg(msg: String): Unit = SwingUtilities.invokeLater(() => {
    val sty =
      if (msg.startsWith("***") || msg.startsWith("==="))  attr(K.GOLD,   bold = true)
      else if (msg.startsWith("[Chat]"))                    attr(new Color(110, 195, 255))
      else if (msg.contains("\u2660") || msg.contains("\u2665") ||
        msg.contains("\u2666") || msg.contains("\u2663")) attr(K.YELLOW, bold = true)
      else if (msg.toLowerCase.contains("wins") ||
        msg.toLowerCase.contains("winner"))          attr(K.GREEN,  bold = true)
      else if (msg.startsWith("[!]"))                       attr(K.RED)
      else if (msg.startsWith("---"))                       attr(new Color(80, 96, 115))
      else                                                  attr(K.SILVER)
    doc.insertString(doc.getLength, msg + "\n", sty)
    if (doc.getLength > 24000) doc.remove(0, 6000)
  })
}

// ═══════════════════════════════════════════════════════════════════════════════
//  ROUNDED BUTTON
// ═══════════════════════════════════════════════════════════════════════════════
class RBtn(label: String, bg: Color, w: Int = 0, h: Int = 34) extends JButton(label) {
  setFont(K.F_BOLD); setForeground(K.WHITE); setBackground(bg)
  setFocusPainted(false); setBorderPainted(false); setOpaque(true)
  setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR))
  if (w > 0) setPreferredSize(new Dimension(w, h))
  override def paintComponent(g: Graphics): Unit = {
    val g2 = g.asInstanceOf[Graphics2D]
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2.setColor(if (getModel.isRollover) bg.brighter() else bg)
    g2.fillRoundRect(0, 0, getWidth, getHeight, 9, 9)
    super.paintComponent(g)
  }
}

// ═══════════════════════════════════════════════════════════════════════════════
//  ADAPTIVE INPUT PANEL
//  Switches layout based on what the server is asking for
// ═══════════════════════════════════════════════════════════════════════════════
class InputPanel(rawSend: String => Unit) extends JPanel(new BorderLayout()) {
  setBackground(K.PANEL)

  // ── Prompt strip ──────────────────────────────────────────────────────────
  private val promptLabel = new JLabel("") { setFont(K.F_BOLD); setForeground(K.GOLD) }
  private val promptBar = new JPanel(new BorderLayout()) {
    setBackground(K.PROMPT_BG)
    setBorder(BorderFactory.createEmptyBorder(7, 14, 7, 14))
    add(new JLabel("\u25ba ") { setFont(K.F_BOLD); setForeground(K.GREEN) }, BorderLayout.WEST)
    add(promptLabel, BorderLayout.CENTER)
    setVisible(false)
  }

  // ── Your cards preview ────────────────────────────────────────────────────
  private var _hand:  Seq[Card] = Nil
  private var _chips: Int       = 0
  private val handPanel = new JPanel {
    setOpaque(false); setPreferredSize(new Dimension(168, 96))
    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      val g2 = g.asInstanceOf[Graphics2D]
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,      RenderingHints.VALUE_ANTIALIAS_ON)
      g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
      val cY = (getHeight - CardR.H) / 2
      if (_hand.nonEmpty) {
        _hand.zipWithIndex.foreach { case (c, i) => CardR.drawCard(g2, 10 + i * (CardR.W + 8), cY, c) }
      } else {
        for (i <- 0 until 2) CardR.drawEmpty(g2, 10 + i * (CardR.W + 8), cY)
      }
      g2.setFont(K.F_SBOLD); g2.setColor(K.GOLD)
      g2.drawString(s"${_chips} chips", 10, getHeight - 6)
    }
  }
  handPanel.setVisible(false)

  // ── CardLayout switcher ───────────────────────────────────────────────────
  private val cl   = new CardLayout()
  private val deck = new JPanel(cl) { setBackground(K.PANEL) }

  // 1. Chat
  private val chatFld = mkField("Send a message or command…")
  private val chatBtn = new RBtn("Send \u25ba", K.FELT, 90)
  chatBtn.addActionListener(_ => doChat()); chatFld.addActionListener(_ => doChat())
  deck.add(wrapRow(chatFld, chatBtn), "chat")

  // 2. Name
  private val nameFld = mkField("Enter your name…")
  private val nameBtn = new RBtn("OK \u25ba", K.BLUE, 80)
  nameBtn.addActionListener(_ => doGeneric(nameFld)); nameFld.addActionListener(_ => doGeneric(nameFld))
  deck.add(wrapRow(nameFld, nameBtn), "name")

  // 3. Session code
  private val codeFld = mkField("4-character session code…")
  private val codeBtn = new RBtn("Join \u25ba", K.BLUE, 90)
  codeBtn.addActionListener(_ => doGeneric(codeFld)); codeFld.addActionListener(_ => doGeneric(codeFld))
  deck.add(wrapRow(codeFld, codeBtn), "code")

  // 4. Main menu
  private val newBtn  = new RBtn("\uD83C\uDFB2  New Game",      K.GREEN, 148)
  private val joinBtn = new RBtn("\uD83D\uDD17  Join Game",      K.BLUE,  148)
  newBtn.addActionListener  (_ => { rawSend("1"); cl.show(deck, "chat") })
  joinBtn.addActionListener (_ => { rawSend("2"); cl.show(deck, "chat") })
  private val menuRow = new JPanel(new FlowLayout(FlowLayout.CENTER, 16, 12)) {
    setBackground(K.PANEL); add(newBtn); add(joinBtn)
  }
  deck.add(menuRow, "menu")

  // 5. Lobby
  private val startBtn = new RBtn("/start \u25ba",  K.GREEN, 112)
  private val botBtn   = new RBtn("+ Bot \uD83E\uDD16", K.BLUE,  112)
  private val leaveBtn = new RBtn("Leave \u2716",   K.RED,   112)
  startBtn.addActionListener(_ => rawSend("/start"))
  botBtn.addActionListener(_   => rawSend("/bot"))
  leaveBtn.addActionListener(_ => rawSend("/wyjdz"))
  private val lchat = mkField("Chat…"); lchat.setPreferredSize(new Dimension(170, 34))
  private val lsend = new RBtn("Chat \u25ba", new Color(48, 60, 76), 88)
  lsend.addActionListener(_ => { val t = lchat.getText.trim; if (t.nonEmpty) { rawSend(t); lchat.setText("") } })
  lchat.addActionListener(_ => { val t = lchat.getText.trim; if (t.nonEmpty) { rawSend(t); lchat.setText("") } })
  private val lobbyRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 9)) {
    setBackground(K.PANEL)
    setBorder(BorderFactory.createEmptyBorder(0, 6, 0, 6))
    add(startBtn); add(botBtn); add(leaveBtn)
    add(sep()); add(lchat); add(lsend)
  }
  deck.add(lobbyRow, "lobby")

  // 6. Game actions
  private val foldBtn  = new RBtn("Fold (F)",       K.RED,   106)
  private val checkBtn = new RBtn("Check (CH)",     K.BLUE,  112)
  private val callBtn  = new RBtn("Call",            K.GREEN, 106)
  private val raiseFld = new JTextField("0") {
    setFont(K.F_BODY); setBackground(K.INPUT_BG); setForeground(K.WHITE); setCaretColor(K.GOLD)
    setBorder(new CompoundBorder(new LineBorder(K.BORDER, 1, true), BorderFactory.createEmptyBorder(5, 8, 5, 8)))
    setPreferredSize(new Dimension(72, 34))
  }
  private val raiseBtn = new RBtn("Raise (R)", K.YELLOW, 106)
  raiseBtn.setForeground(new Color(26, 20, 4))

  foldBtn.addActionListener(_  => act("f"))
  checkBtn.addActionListener(_ => act("ch"))
  callBtn.addActionListener(_  => act("c"))
  raiseBtn.addActionListener(_ => act("r " + raiseFld.getText.trim))
  raiseFld.addActionListener(_ => act("r " + raiseFld.getText.trim))

  private val actionRow = new JPanel(new FlowLayout(FlowLayout.CENTER, 8, 9)) {
    setBackground(K.PANEL)
    add(foldBtn); add(checkBtn); add(callBtn); add(sep())
    add(new JLabel("Amount:") { setFont(K.F_SMALL); setForeground(K.SILVER) })
    add(raiseFld); add(raiseBtn)
  }
  deck.add(actionRow, "action")

  // ── Assemble ──────────────────────────────────────────────────────────────
  private val bottom = new JPanel(new BorderLayout()) {
    setBackground(K.PANEL)
    setBorder(new MatteBorder(1, 0, 0, 0, K.BORDER))
    add(promptBar,  BorderLayout.NORTH)
    add(handPanel,  BorderLayout.WEST)
    add(deck,       BorderLayout.CENTER)
  }
  add(bottom, BorderLayout.CENTER)

  // ── Public API ────────────────────────────────────────────────────────────
  def onPrompt(p: String): Unit = SwingUtilities.invokeLater(() => {
    promptLabel.setText(p); promptBar.setVisible(true)
    if      (Parser.isNamePrompt(p))   { cl.show(deck, "name");   nameFld.requestFocusInWindow() }
    else if (Parser.isCodePrompt(p))   { cl.show(deck, "code");   codeFld.requestFocusInWindow() }
    else if (Parser.isMenuPrompt(p))   { cl.show(deck, "menu") }
    else if (Parser.isActionPrompt(p)) { cl.show(deck, "action"); raiseFld.requestFocusInWindow() }
    else                               { cl.show(deck, "chat");   chatFld.requestFocusInWindow() }
  })

  def showLobby(): Unit  = SwingUtilities.invokeLater(() => { promptBar.setVisible(false); cl.show(deck, "lobby") })
  def showGame(): Unit   = SwingUtilities.invokeLater(() => handPanel.setVisible(true))
  def hidePrompt(): Unit = SwingUtilities.invokeLater(() => promptBar.setVisible(false))

  def syncHand(hand: Seq[Card], chips: Int): Unit = SwingUtilities.invokeLater(() => {
    _hand = hand; _chips = chips; handPanel.repaint()
  })
  def syncCallAmount(n: Int): Unit = SwingUtilities.invokeLater(() =>
    callBtn.setText(if (n > 0) s"Call $n" else "Call"))
  def syncMinRaise(n: Int): Unit = SwingUtilities.invokeLater(() =>
    if (n > 0) raiseFld.setText(n.toString))

  // ── Private helpers ───────────────────────────────────────────────────────
  private def act(cmd: String): Unit = {
    rawSend(cmd); promptBar.setVisible(false); cl.show(deck, "chat")
  }
  private def doGeneric(f: JTextField): Unit = {
    val t = f.getText.trim; if (t.nonEmpty) rawSend(t); f.setText("")
    promptBar.setVisible(false); cl.show(deck, "chat")
  }
  private def doChat(): Unit = {
    val t = chatFld.getText.trim; if (t.nonEmpty) { rawSend(t); chatFld.setText("") }
  }
  private def mkField(hint: String = ""): JTextField = new JTextField() {
    setFont(K.F_BODY); setBackground(K.INPUT_BG); setForeground(K.WHITE)
    setCaretColor(K.GOLD); setToolTipText(hint)
    setBorder(new CompoundBorder(new LineBorder(K.BORDER, 1, true), BorderFactory.createEmptyBorder(6, 10, 6, 10)))
    setPreferredSize(new Dimension(220, 34))
  }
  private def wrapRow(f: JTextField, b: JButton): JPanel = new JPanel(new BorderLayout(8, 0)) {
    setBackground(K.PANEL); setBorder(BorderFactory.createEmptyBorder(8, 12, 8, 12))
    add(f, BorderLayout.CENTER); add(b, BorderLayout.EAST)
  }
  private def sep(): JSeparator = new JSeparator(SwingConstants.VERTICAL) {
    setPreferredSize(new Dimension(1, 30)); setForeground(K.BORDER)
  }
}

// ═══════════════════════════════════════════════════════════════════════════════
//  CONNECTION SCREEN
// ═══════════════════════════════════════════════════════════════════════════════
class ConnectScreen(onConnect: (String, Int) => Unit) extends JPanel(new GridBagLayout()) {
  setBackground(K.BG)

  private val inner = new JPanel(new GridBagLayout()) {
    setBackground(K.PANEL)
    setBorder(new CompoundBorder(
      new LineBorder(K.GOLD, 1, true),
      BorderFactory.createEmptyBorder(28, 36, 28, 36)))
  }

  private def gbc(r: Int, c: Int, fill: Int = GridBagConstraints.HORIZONTAL,
                  span: Int = 1, anchor: Int = GridBagConstraints.CENTER): GridBagConstraints = {
    val g = new GridBagConstraints()
    g.gridx = c; g.gridy = r; g.fill = fill; g.gridwidth = span
    g.anchor = anchor; g.insets = new Insets(6, 6, 6, 6); g.weightx = 1.0; g
  }

  private def lbl(t: String, f: Font, fg: Color, align: Int = SwingConstants.LEFT): JLabel =
    new JLabel(t) { setFont(f); setForeground(fg); setHorizontalAlignment(align) }

  private def fld(d: String): JTextField = new JTextField(d) {
    setFont(K.F_BODY); setBackground(K.INPUT_BG); setForeground(K.WHITE); setCaretColor(K.GOLD)
    setBorder(new CompoundBorder(new LineBorder(K.BORDER, 1, true), BorderFactory.createEmptyBorder(7, 10, 7, 10)))
  }

  private val titleLbl  = lbl("\u2660  POKER ONLINE  \u2660", new Font("Georgia", Font.BOLD, 22), K.GOLD, SwingConstants.CENTER)
  private val subLbl    = lbl("Connect to a game server", K.F_SMALL, K.SILVER, SwingConstants.CENTER)
  private val hostField = fld("localhost")
  private val portField = fld("8080")
  private val statusLbl = lbl("", K.F_SMALL, K.RED, SwingConstants.CENTER)
  private val connectBtn = new RBtn("Connect to Server", K.FELT, 200, 42)

  inner.add(titleLbl,   gbc(0, 0, span = 2))
  inner.add(subLbl,     gbc(1, 0, span = 2))
  inner.add(lbl("Host:", K.F_BOLD, K.SILVER), gbc(2, 0, GridBagConstraints.NONE, anchor = GridBagConstraints.EAST))
  inner.add(hostField,  gbc(2, 1))
  inner.add(lbl("Port:", K.F_BOLD, K.SILVER), gbc(3, 0, GridBagConstraints.NONE, anchor = GridBagConstraints.EAST))
  inner.add(portField,  gbc(3, 1))
  inner.add(connectBtn, gbc(4, 0, GridBagConstraints.CENTER, 2))
  inner.add(statusLbl,  gbc(5, 0, span = 2))

  val outer = new GridBagConstraints()
  outer.fill = GridBagConstraints.NONE; outer.anchor = GridBagConstraints.CENTER
  outer.weightx = 1.0; outer.weighty = 1.0
  add(inner, outer)

  connectBtn.addActionListener(_ => tryConnect())
  portField.addActionListener(_ => tryConnect())
  hostField.addActionListener(_ => tryConnect())

  def setStatus(msg: String, fg: Color): Unit = SwingUtilities.invokeLater(() => {
    statusLbl.setForeground(fg); statusLbl.setText(msg)
  })

  private def tryConnect(): Unit = {
    val host = hostField.getText.trim
    try {
      val port = portField.getText.trim.toInt
      setStatus("Connecting…", K.GOLD); onConnect(host, port)
    } catch { case _: NumberFormatException => setStatus("Invalid port number!", K.RED) }
  }
}

// ═══════════════════════════════════════════════════════════════════════════════
//  HEADER BAR
// ═══════════════════════════════════════════════════════════════════════════════
class HeaderBar extends JPanel(new BorderLayout()) {
  setBackground(new Color(11, 14, 19))
  setBorder(new CompoundBorder(
    new MatteBorder(0, 0, 1, 0, K.BORDER),
    BorderFactory.createEmptyBorder(10, 16, 10, 16)))
  setPreferredSize(new Dimension(0, 48))

  private val title = new JLabel("  \u2660  POKER ONLINE") {
    setFont(new Font("Georgia", Font.BOLD, 16)); setForeground(K.GOLD)
  }
  val statusLbl = new JLabel("Offline") { setFont(K.F_SMALL); setForeground(K.SILVER) }
  val sessionLbl = new JLabel("") {
    setFont(new Font("Monospaced", Font.BOLD, 13)); setForeground(K.GOLD)
    setBorder(new CompoundBorder(new LineBorder(K.GOLD_DIM, 1, true), BorderFactory.createEmptyBorder(3, 8, 3, 8)))
    setVisible(false)
  }
  private val right = new JPanel(new FlowLayout(FlowLayout.RIGHT, 10, 0)) {
    setOpaque(false); add(sessionLbl); add(statusLbl)
  }
  add(title, BorderLayout.WEST); add(right, BorderLayout.EAST)

  def setOnline(host: String, port: Int): Unit = SwingUtilities.invokeLater(() => {
    statusLbl.setText(s"\u25cf $host:$port"); statusLbl.setForeground(K.GREEN)
  })
  def setOffline(): Unit = SwingUtilities.invokeLater(() => {
    statusLbl.setText("\u25cf Disconnected"); statusLbl.setForeground(K.RED)
    sessionLbl.setVisible(false)
  })
  def setSession(code: String): Unit = SwingUtilities.invokeLater(() => {
    if (code.nonEmpty) { sessionLbl.setText(s"  SESSION: $code  "); sessionLbl.setVisible(true) }
  })
}

// ═══════════════════════════════════════════════════════════════════════════════
//  MAIN APPLICATION
// ═══════════════════════════════════════════════════════════════════════════════
object PekkoClientUI {

  private lazy val system: ActorSystem[Nothing] =
    ActorSystem(Behaviors.empty, "PokerClientUI")

  def main(args: Array[String]): Unit = {
    try { UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName) }
    catch { case _: Exception => }
    UIManager.put("Panel.background",             K.BG)
    UIManager.put("OptionPane.background",        K.PANEL)
    UIManager.put("OptionPane.messageForeground",  K.WHITE)
    SwingUtilities.invokeLater(() => buildWindow())
  }

  private def buildWindow(): Unit = {
    val frame = new JFrame("Poker Online")
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.setPreferredSize(new Dimension(1060, 700))
    frame.setMinimumSize(new Dimension(800, 560))

    val st     = new State
    val table  = new TablePanel(st)
    val log    = new LogPanel
    val header = new HeaderBar
    var sendFn: String => Unit = _ => ()

    val input = new InputPanel((msg: String) => sendFn(msg))

    val split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, table, log) {
      setDividerLocation(800); setResizeWeight(1.0)
      setBorder(null); setDividerSize(4); setBackground(K.BG)
    }

    val gameScreen = new JPanel(new BorderLayout()) {
      setBackground(K.BG)
      add(header, BorderLayout.NORTH)
      add(split,  BorderLayout.CENTER)
      add(input,  BorderLayout.SOUTH)
    }

    val rootLayout = new CardLayout()
    val root = new JPanel(rootLayout) { setBackground(K.BG) }

    var connectScreen: ConnectScreen = null
    connectScreen = new ConnectScreen((host: String, port: Int) =>
      connectWS(host, port, st, table, log, input, header,
        onReady = (fn: String => Unit) => {
          sendFn = fn
          SwingUtilities.invokeLater(() => {
            rootLayout.show(root, "game")
            frame.setTitle(s"Poker Online  \u2013  $host:$port")
          })
        },
        onError = (msg: String) => connectScreen.setStatus(msg, K.RED),
        onClose = () => header.setOffline()
      )
    )

    root.add(connectScreen, "connect")
    root.add(gameScreen,    "game")
    rootLayout.show(root, "connect")

    frame.setContentPane(root)
    frame.pack()
    frame.setLocationRelativeTo(null)
    frame.setVisible(true)
  }

  private def connectWS(
                         host:    String,
                         port:    Int,
                         st:      State,
                         table:   TablePanel,
                         log:     LogPanel,
                         input:   InputPanel,
                         header:  HeaderBar,
                         onReady: (String => Unit) => Unit,
                         onError: String => Unit,
                         onClose: () => Unit
                       ): Unit = {
    implicit val sys: ActorSystem[Nothing] = system
    import sys.executionContext

    val (queue, source) =
      Source.queue[Message](256, OverflowStrategy.dropTail).preMaterialize()

    val rawSend: String => Unit = (text: String) => { queue.offer(TextMessage(s"INPUT:$text")); () }

    val sink = Sink.foreach[Message] {
      case TextMessage.Strict(raw) if raw.startsWith("MSG:") =>
        val msg = raw.drop(4)
        log.addMsg(msg)
        Parser.process(msg, st)

        // Keep header session badge up to date
        if (st.sessionCode.nonEmpty) header.setSession(st.sessionCode)

        // Register local player once chips appear (first chip message = we are seated)
        if (st.myChips > 0 && st.localSeat.isEmpty) st.addPlayer("You", local = true)
        st.localSeat.foreach { s => s.chips = st.myChips; s.hand = st.myHand }

        input.syncHand(st.myHand, st.myChips)
        input.syncCallAmount(st.callAmount)
        input.syncMinRaise(st.minRaise)

        if (st.inLobby) input.showLobby() else input.showGame()
        table.refresh()

      case TextMessage.Strict(raw) if raw.startsWith("PROMPT:") =>
        input.onPrompt(raw.drop(7))

      case _ =>
    }

    val flow = Flow.fromSinkAndSource(sink, source).watchTermination()(Keep.right)
    val (upgrade, closed) = Http().singleWebSocketRequest(
      WebSocketRequest(s"ws://$host:$port/poker"), flow)

    upgrade.foreach { resp =>
      if (resp.response.status.isSuccess()) {
        log.addMsg(s"*** Connected to $host:$port ***")
        header.setOnline(host, port)
        onReady(rawSend)
      }
    }
    upgrade.failed.foreach(ex => onError(s"Connection failed: ${ex.getMessage}"))
    closed.onComplete(_ => onClose())
  }
}