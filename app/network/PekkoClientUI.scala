package network

import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import org.apache.pekko.stream.OverflowStrategy
import org.apache.pekko.stream.scaladsl.{Flow, Keep, Sink, Source}

import java.awt._
import java.awt.event._
import java.awt.geom._
import javax.swing._
import javax.swing.border._
import javax.swing.text._
import scala.collection.mutable
import scala.util.matching.Regex

// ═══════════════════════════════════════════════════════════════════════════════
//  COLOUR + FONT PALETTE
// ═══════════════════════════════════════════════════════════════════════════════
object K {
  val BG        = new Color(13, 17, 23)
  val PANEL     = new Color(22, 28, 38)
  val BORDER    = new Color(40, 52, 66)
  val FELT      = new Color(22, 92, 52)
  val FELT_EDGE = new Color(14, 62, 34)
  val FELT_HIGH = new Color(30, 115, 65)
  val WOOD      = new Color(95, 58, 18)
  val WOOD_DARK = new Color(55, 32, 8)
  val GOLD      = new Color(212, 175, 55)
  val GOLD_DIM  = new Color(140, 112, 30)
  val WHITE     = new Color(235, 240, 248)
  val SILVER    = new Color(155, 168, 185)
  val RED       = new Color(210, 52, 52)
  val GREEN     = new Color(48, 170, 72)
  val BLUE      = new Color(52, 122, 215)
  val YELLOW    = new Color(218, 192, 48)
  val ACT_GL    = new Color(48, 170, 72, 55)
  val ACT_BD    = new Color(48, 170, 72)
  val CARD_BK   = new Color(28, 50, 130)
  val INPUT_BG  = new Color(26, 34, 46)
  val PROMPT_BG = new Color(18, 44, 28)

  val F_BODY  = new Font("Segoe UI",   Font.PLAIN, 13)
  val F_BOLD  = new Font("Segoe UI",   Font.BOLD,  13)
  val F_SMALL = new Font("Segoe UI",   Font.PLAIN, 11)
  val F_SBOLD = new Font("Segoe UI",   Font.BOLD,  11)
  val F_TITLE = new Font("Segoe UI",   Font.BOLD,  16)
  val F_MONO  = new Font("Monospaced", Font.PLAIN, 11)
  val F_GEO   = new Font("Georgia",    Font.BOLD,  16)
}

// ═══════════════════════════════════════════════════════════════════════════════
//  DATA MODEL
// ═══════════════════════════════════════════════════════════════════════════════
case class Card(rank: String, suit: String) {
  val isRed: Boolean = suit == "\u2665" || suit == "\u2666"
  override def toString: String = rank + suit
}
object Card {
  private val re = """([2-9TJQKA]|10)([\u2660\u2665\u2666\u2663])""".r
  def allIn(s: String): Seq[Card] =
    re.findAllMatchIn(s).map(m => Card(m.group(1), m.group(2))).toSeq
}

class Seat {
  var name:     String    = ""
  var chips:    Int       = 0
  var bet:      Int       = 0
  var cards:    Int       = 0
  var hand:     Seq[Card] = Nil
  var active:   Boolean   = false
  var folded:   Boolean   = false
  var occupied: Boolean   = false
  var isLocal:  Boolean   = false
  def reset(): Unit = {
    folded = false; bet = 0; active = false; hand = Nil
    if (occupied && !isLocal) cards = 2
  }
}

class State {
  val seats: Array[Seat]  = Array.fill(8)(new Seat())
  var community: Seq[Card] = Nil
  var pot:        Int     = 0
  var myChips:    Int     = 0
  var myHand:     Seq[Card] = Nil
  var round:      String  = "Lobby"
  var sessionCode:String  = ""
  var callAmount: Int     = 0
  var minRaise:   Int     = 0
  var inLobby:    Boolean = true

  def localSeat: Option[Seat] = seats.find(_.isLocal)

  def reset(): Unit = {
    community = Nil; pot = 0; myHand = Nil; callAmount = 0; minRaise = 0
    seats.foreach(_.reset())
  }
  def addPlayer(name: String, local: Boolean = false): Unit =
    seats.find(!_.occupied).foreach { s =>
      s.name = name; s.occupied = true; s.isLocal = local
      s.cards = if (local) 0 else 2
    }
  def removePlayer(name: String): Unit =
    seats.find(s => s.occupied && s.name == name).foreach { s =>
      s.occupied = false; s.name = ""; s.hand = Nil; s.cards = 0
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
//  MESSAGE PARSER
// ═══════════════════════════════════════════════════════════════════════════════
object Parser {
  val potRe      = """(?i)(?:pot|pula)[:\s]+(\d+)""".r
  val chipsRe    = """(?i)(?:zeton[oy]?|chips?)[:\s]+(\d+)""".r
  val callRe     = """(?i)call[^\(]*\((\d+)\)""".r
  val minRaiseRe = """(?i)min[:\s]+(\d+)""".r
  val myCardRe   = """(?i)(?:twoje\s+karty|hand)[:\s]+(.+)""".r
  val tableRe    = """(?i)(?:stol|community|flop|turn|river)[:\s]+(.+)""".r
  val joinRe     = """(?i)\*{3}\s*(.+?)\s+dolaczyl""".r
  val leftRe     = """(?i)\[([^\]]+) opuscil""".r
  val sessionRe  = """(?i)SESJ[AI][:\s]+([A-Z0-9]{4})""".r
  val foldRe     = """(?i)\[([^\]]+)\]\s+(?:pasuje|fold)""".r
  val betRe = """(?i)\[([^\]]+)\]\s+(?:bets?|raises?)[^\d]*(\d+)""".r
  val newRoundRe = """(?i)new\s+round|new\s+hand|hand\s+#""".r
  val winRe = """(?i)(?:wins?|winner)""".r

  def process(msg: String, st: State): Unit = {
    sessionRe.findFirstMatchIn(msg).foreach(m => st.sessionCode = m.group(1))
    if (newRoundRe.findFirstIn(msg).isDefined) { st.reset(); st.round = "Pre-Flop" }

    potRe.findFirstMatchIn(msg)     .foreach(m => st.pot        = m.group(1).toInt)
    chipsRe.findFirstMatchIn(msg)   .foreach(m => st.myChips    = m.group(1).toInt)
    callRe.findFirstMatchIn(msg)    .foreach(m => st.callAmount = m.group(1).toInt)
    minRaiseRe.findFirstMatchIn(msg).foreach(m => st.minRaise   = m.group(1).toInt)

    myCardRe.findFirstMatchIn(msg).foreach { m =>
      val c = Card.allIn(m.group(1))
      if (c.nonEmpty) { st.myHand = c; st.localSeat.foreach(_.hand = c) }
    }
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
    joinRe.findFirstMatchIn(msg).foreach(m => st.addPlayer(m.group(1)))
    leftRe.findFirstMatchIn(msg).foreach(m => st.removePlayer(m.group(1)))
    foldRe.findFirstMatchIn(msg).foreach { m =>
      st.seats.find(_.name == m.group(1)).foreach { s =>
        s.folded = true; s.active = false; s.cards = 0
      }
    }
    betRe.findFirstMatchIn(msg).foreach { m =>
      st.seats.find(_.name == m.group(1)).foreach { s =>
        s.bet = m.group(2).toInt; s.active = false
      }
    }
    if (msg.contains("LOBBY") || msg.contains("dolaczyl do pokoju")) st.inLobby = true
    if (msg.contains("ROZPOCZYNAMY") || newRoundRe.findFirstIn(msg).isDefined) st.inLobby = false
    if (winRe.findFirstIn(msg).isDefined) st.round = "Showdown"
  }

  def isActionPrompt(p: String): Boolean = {
    val lo = p.toLowerCase
    lo.contains("fold") || lo.contains("call") || lo.contains("raise") || lo.contains("akcj")
  }
  def isMenuPrompt(p: String): Boolean = p.contains("1") && p.contains("2") &&
    (p.toLowerCase.contains("wybierz") || p.toLowerCase.contains("menu"))
  def isNamePrompt(p: String): Boolean = {
    val lo = p.toLowerCase
    lo.contains("imi") || lo.contains("name")
  }
  def isCodePrompt(p: String): Boolean = {
    val lo = p.toLowerCase
    lo.contains("kod") || lo.contains("code")
  }
}

// ═══════════════════════════════════════════════════════════════════════════════
//  CARD RENDERER
// ═══════════════════════════════════════════════════════════════════════════════
object CardR {
  val W = 62; val H = 92
  private val rankFnt = new Font("Georgia",  Font.BOLD,  11)
  private val bigFnt  = new Font("Segoe UI", Font.PLAIN, 22)
  private val smFnt   = new Font("Georgia",  Font.BOLD,   9)

  def drawCard(g: Graphics2D, x: Int, y: Int, c: Card): Unit = {
    val fg = if (c.isRed) new Color(195, 38, 38) else new Color(18, 18, 26)
    dropShadow(g, x, y)
    g.setColor(Color.WHITE); g.fillRoundRect(x, y, W, H, 8, 8)
    g.setColor(new Color(190, 195, 205)); g.drawRoundRect(x, y, W, H, 8, 8)
    g.setColor(fg)
    g.setFont(smFnt);  g.drawString(c.rank, x + 5, y + 13)
    g.setFont(new Font("Segoe UI", Font.PLAIN, 9)); g.drawString(c.suit, x + 5, y + 23)
    g.setFont(bigFnt)
    val bfm = g.getFontMetrics
    g.drawString(c.suit, x + (W - bfm.stringWidth(c.suit)) / 2, y + H / 2 + 9)
    g.setFont(smFnt)
    val rfm = g.getFontMetrics(smFnt)
    g.drawString(c.rank, x + W - rfm.stringWidth(c.rank) - 5, y + H - 6)
  }

  def drawBack(g: Graphics2D, x: Int, y: Int): Unit = {
    dropShadow(g, x, y)
    g.setColor(K.CARD_BK); g.fillRoundRect(x, y, W, H, 8, 8)
    g.setColor(new Color(255, 255, 255, 30)); g.drawRoundRect(x + 3, y + 3, W - 6, H - 6, 5, 5)
    g.setColor(new Color(255, 255, 255, 14))
    for (i <- 0 until 4; j <- 0 until 5) g.drawOval(x + 7 + i * 11, y + 7 + j * 13, 7, 7)
    g.setColor(new Color(255, 255, 255, 50)); g.drawRoundRect(x, y, W, H, 8, 8)
  }

  def drawEmpty(g: Graphics2D, x: Int, y: Int): Unit = {
    g.setColor(new Color(255, 255, 255, 26))
    val s0 = g.getStroke
    g.setStroke(new BasicStroke(1.2f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND,
      1f, Array(4f, 4f), 0f))
    g.drawRoundRect(x, y, W, H, 8, 8)
    g.setStroke(s0)
  }

  private def dropShadow(g: Graphics2D, x: Int, y: Int): Unit = {
    g.setColor(new Color(0, 0, 0, 60))
    g.fillRoundRect(x + 3, y + 4, W, H, 8, 8)
  }
}

// ═══════════════════════════════════════════════════════════════════════════════
//  TABLE PANEL  –  custom-painted oval poker table
// ═══════════════════════════════════════════════════════════════════════════════
class TablePanel(val st: State) extends JPanel {
  setOpaque(true); setBackground(K.BG)

  private val nmFnt  = new Font("Segoe UI", Font.BOLD,  12)
  private val cpFnt  = new Font("Segoe UI", Font.PLAIN, 11)
  private val potFnt = new Font("Segoe UI", Font.BOLD,  14)
  private val rndFnt = new Font("Segoe UI", Font.BOLD,  13)
  private val bdFnt  = new Font("Segoe UI", Font.BOLD,   9)

  // Seat 0 = bottom (local player), rest around the oval
  private val seatDeg = Array(270.0, 330.0, 30.0, 90.0, 150.0, 210.0, 252.0, 288.0)

  private def seatPos(idx: Int, w: Int, h: Int): (Int, Int) = {
    val a = seatDeg(idx % seatDeg.length).toRadians
    (w / 2 + ((w * 0.40) * Math.cos(a)).toInt,
      h / 2 + ((h * 0.37) * Math.sin(a)).toInt)
  }

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    val g2 = g.asInstanceOf[Graphics2D]
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,      RenderingHints.VALUE_ANTIALIAS_ON)
    g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    val w = getWidth; val h = getHeight
    paintTable(g2, w, h)
    paintCommunity(g2, w, h)
    paintPot(g2, w, h)
    paintRound(g2, w, h)
    paintSeats(g2, w, h)
  }

  private def paintTable(g2: Graphics2D, w: Int, h: Int): Unit = {
    val cx = w / 2; val cy = h / 2
    val rx = (w * 0.43).toInt; val ry = (h * 0.41).toInt
    g2.setColor(new Color(0, 0, 0, 85))
    g2.fillOval(cx - rx - 6, cy - ry + 12, (rx + 6) * 2, (ry + 8) * 2)
    g2.setPaint(new GradientPaint(0f, 0f, K.WOOD, w.toFloat, h.toFloat, K.WOOD_DARK))
    g2.fillOval(cx - rx - 14, cy - ry - 14, (rx + 14) * 2, (ry + 14) * 2)
    g2.setColor(new Color(255, 200, 120, 40))
    g2.setStroke(new BasicStroke(2.5f))
    g2.drawOval(cx - rx - 14, cy - ry - 14, (rx + 14) * 2, (ry + 14) * 2)
    g2.setStroke(new BasicStroke(1))
    g2.setPaint(new RadialGradientPaint(
      cx.toFloat, cy.toFloat, rx.toFloat,
      Array(0f, 0.65f, 1f),
      Array(K.FELT_HIGH, K.FELT, K.FELT_EDGE)
    ))
    g2.fillOval(cx - rx, cy - ry, rx * 2, ry * 2)
    g2.setColor(new Color(255, 255, 255, 16)); g2.setStroke(new BasicStroke(2))
    g2.drawOval(cx - rx + 6, cy - ry + 6, rx * 2 - 12, ry * 2 - 12)
    g2.setStroke(new BasicStroke(1))
  }

  private def paintCommunity(g2: Graphics2D, w: Int, h: Int): Unit = {
    val cx = w / 2; val cy = h / 2
    val total = 5 * (CardR.W + 6) - 6
    val sx = cx - total / 2; val sy = cy - CardR.H / 2 - 6
    for (i <- 0 until 5) {
      val x = sx + i * (CardR.W + 6)
      if (i < st.community.size) CardR.drawCard(g2, x, sy, st.community(i))
      else                        CardR.drawEmpty(g2, x, sy)
    }
  }

  private def paintPot(g2: Graphics2D, w: Int, h: Int): Unit = {
    if (st.pot <= 0) return
    val cx = w / 2; val cy = h / 2
    val txt = s"Pot: ${st.pot}"
    g2.setFont(potFnt)
    val fm = g2.getFontMetrics
    val tx = cx - fm.stringWidth(txt) / 2
    val ty = cy + CardR.H / 2 + 30
    g2.setColor(K.GOLD); g2.fillOval(tx - 22, ty - 13, 15, 15)
    g2.setColor(K.GOLD_DIM); g2.drawOval(tx - 22, ty - 13, 15, 15)
    g2.setColor(new Color(0, 0, 0, 110)); g2.drawString(txt, tx + 1, ty + 1)
    g2.setColor(K.GOLD); g2.drawString(txt, tx, ty)
  }

  private def paintRound(g2: Graphics2D, w: Int, h: Int): Unit = {
    val cx = w / 2; val cy = h / 2
    g2.setFont(rndFnt)
    val fm = g2.getFontMetrics
    val tx = cx - fm.stringWidth(st.round) / 2
    val ty = cy - CardR.H / 2 - 22
    g2.setColor(new Color(0, 0, 0, 100)); g2.drawString(st.round, tx + 1, ty + 1)
    g2.setColor(new Color(190, 215, 195, 175)); g2.drawString(st.round, tx, ty)
  }

  private def paintSeats(g2: Graphics2D, w: Int, h: Int): Unit = {
    val occ     = st.seats.filter(_.occupied)
    val localI  = occ.indexWhere(_.isLocal)
    val ordered = if (localI >= 0) occ.drop(localI) ++ occ.take(localI) else occ
    ordered.zipWithIndex.foreach { case (s, i) =>
      if (i < seatDeg.length) {
        val (px, py) = seatPos(i, w, h)
        paintSeat(g2, s, px, py, isBottom = (i == 0))
      }
    }
  }

  private def paintSeat(g2: Graphics2D, s: Seat, cx: Int, cy: Int, isBottom: Boolean): Unit = {
    val pw = 116; val ph = 46
    val px = cx - pw / 2
    val py = if (isBottom) cy - ph - 5 else cy + 5

    if (s.active) {
      g2.setColor(K.ACT_GL); g2.fillRoundRect(px - 5, py - 5, pw + 10, ph + 10, 15, 15)
      val s0 = g2.getStroke; g2.setColor(K.ACT_BD); g2.setStroke(new BasicStroke(2.2f))
      g2.drawRoundRect(px - 5, py - 5, pw + 10, ph + 10, 15, 15); g2.setStroke(s0)
    }

    val bg = if (s.folded) new Color(38, 38, 50, 215) else new Color(18, 24, 34, 220)
    g2.setColor(bg); g2.fillRoundRect(px, py, pw, ph, 10, 10)
    g2.setColor(if (s.active) K.ACT_BD else K.BORDER); g2.drawRoundRect(px, py, pw, ph, 10, 10)

    g2.setFont(nmFnt); g2.setColor(if (s.folded) K.SILVER else K.WHITE)
    val nm = if (s.name.length > 12) s.name.take(12) + "\u2026" else s.name
    g2.drawString(nm, px + 8, py + 18)

    g2.setFont(cpFnt); g2.setColor(K.GOLD)
    g2.drawString(s.chips.toString, px + 8, py + 35)
    g2.setFont(new Font("Segoe UI", Font.PLAIN, 9)); g2.setColor(K.GOLD_DIM)
    g2.drawString(" \u017c", px + 8 + g2.getFontMetrics(cpFnt).stringWidth(s.chips.toString), py + 35)

    if (s.folded) {
      g2.setFont(K.F_SBOLD); g2.setColor(K.RED)
      g2.drawString("FOLD", px + pw - 38, py + 18)
    }

    val gap  = 8; val hW = 2 * CardR.W + gap
    val cX   = cx - hW / 2
    val cY   = if (isBottom) py - CardR.H - 8 else py + ph + 8

    if (s.hand.nonEmpty)
      s.hand.zipWithIndex.foreach { case (c, i) => CardR.drawCard(g2, cX + i * (CardR.W + gap), cY, c) }
    else if (s.cards > 0 && !s.folded)
      for (i <- 0 until s.cards.min(2)) CardR.drawBack(g2, cX + i * (CardR.W + gap), cY)

    if (s.bet > 0) {
      val bx = if (isBottom) cx + 62 else cx - 36
      val by = if (isBottom) cy - 38  else cy + 8
      g2.setColor(K.YELLOW); g2.fillOval(bx, by, 24, 24)
      g2.setColor(new Color(140, 120, 22)); g2.drawOval(bx, by, 24, 24)
      g2.setFont(bdFnt); g2.setColor(Color.BLACK)
      val bt = s.bet.toString; val bfm = g2.getFontMetrics
      g2.drawString(bt, bx + (24 - bfm.stringWidth(bt)) / 2, by + 15)
    }
  }

  def refresh(): Unit = SwingUtilities.invokeLater(() => repaint())
}

// ═══════════════════════════════════════════════════════════════════════════════
//  GAME LOG PANEL
// ═══════════════════════════════════════════════════════════════════════════════
class LogPanel extends JPanel(new BorderLayout()) {
  setBackground(K.PANEL)
  setPreferredSize(new Dimension(228, 0))
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
  private val header = new JLabel("  \u2663  LOG GRY") {
    setFont(K.F_SBOLD); setForeground(K.GOLD); setOpaque(true)
    setBackground(new Color(18, 24, 34))
    setBorder(BorderFactory.createEmptyBorder(9, 8, 9, 8))
  }
  add(header, BorderLayout.NORTH); add(scroll, BorderLayout.CENTER)

  private val doc = pane.getStyledDocument
  private def attr(fg: Color, bold: Boolean = false): SimpleAttributeSet = {
    val a = new SimpleAttributeSet()
    StyleConstants.setForeground(a, fg); StyleConstants.setBold(a, bold)
    StyleConstants.setFontFamily(a, K.F_MONO.getFamily)
    StyleConstants.setFontSize(a, K.F_MONO.getSize); a
  }

  def addMsg(msg: String): Unit = SwingUtilities.invokeLater(() => {
    val sty =
      if (msg.startsWith("***"))          attr(K.GOLD,  bold = true)
      else if (msg.startsWith("[Chat]"))  attr(new Color(110, 195, 255))
      else if (msg.contains("\u2660") || msg.contains("\u2665") ||
        msg.contains("\u2666") || msg.contains("\u2663")) attr(K.YELLOW, bold = true)
      else if (msg.toLowerCase.contains("wygryw")) attr(K.GREEN, bold = true)
      else if (msg.startsWith("[!]"))     attr(K.RED)
      else if (msg.startsWith("---") || msg.startsWith("===")) attr(new Color(80, 96, 115))
      else                               attr(K.SILVER)
    doc.insertString(doc.getLength, msg + "\n", sty)
    if (doc.getLength > 24000) doc.remove(0, 6000)
  })
}

// ═══════════════════════════════════════════════════════════════════════════════
//  ROUNDED BUTTON helper
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
//  Switches between: lobby controls | action buttons | name / code / menu / chat
// ═══════════════════════════════════════════════════════════════════════════════
class InputPanel(rawSend: String => Unit) extends JPanel(new BorderLayout()) {
  setBackground(K.PANEL)

  // ── Prompt strip ───────────────────────────────────────────────────────────
  private val promptLabel = new JLabel("") {
    setFont(K.F_BOLD); setForeground(K.GOLD)
  }
  private val promptBar = new JPanel(new BorderLayout()) {
    setBackground(K.PROMPT_BG)
    setBorder(BorderFactory.createEmptyBorder(7, 14, 7, 14))
    add(new JLabel("\u25ba ") { setFont(K.F_BOLD); setForeground(K.GREEN) }, BorderLayout.WEST)
    add(promptLabel, BorderLayout.CENTER)
    setVisible(false)
  }

  // ── Your cards strip (shows hand + chips) ─────────────────────────────────
  private var _hand:  Seq[Card] = Nil
  private var _chips: Int       = 0
  private val handPanel = new JPanel {
    setOpaque(false); setPreferredSize(new Dimension(165, 90))
    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      val g2 = g.asInstanceOf[Graphics2D]
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
      val cY = (getHeight - CardR.H) / 2
      _hand.zipWithIndex.foreach { case (c, i) => CardR.drawCard(g2, 10 + i * (CardR.W + 8), cY, c) }
      if (_hand.isEmpty) for (i <- 0 until 2) CardR.drawEmpty(g2, 10 + i * (CardR.W + 8), cY)
      g2.setFont(K.F_SBOLD); g2.setColor(K.GOLD)
      g2.drawString(s"${_chips} \u017c", 10, getHeight - 8)
    }
  }
  handPanel.setVisible(false)

  // ── Switcher panels (CardLayout) ───────────────────────────────────────────
  private val cl   = new CardLayout()
  private val deck = new JPanel(cl) { setBackground(K.PANEL) }

  // 1. Chat
  private val chatFld = mkField()
  private val chatBtn = new RBtn("Wy\u015blij \u25ba", K.FELT, 96)
  chatBtn.addActionListener(_ => doChat())
  chatFld.addActionListener(_ => doChat())
  deck.add(wrap(chatFld, chatBtn), "chat")

  // 2. Name
  private val nameFld = mkField()
  private val nameBtn = new RBtn("OK \u25ba", K.BLUE, 80)
  nameBtn.addActionListener(_ => doGeneric(nameFld))
  nameFld.addActionListener(_ => doGeneric(nameFld))
  deck.add(wrap(nameFld, nameBtn), "name")

  // 3. Code
  private val codeFld = mkField()
  private val codeBtn = new RBtn("Do\u0142\u0105cz \u25ba", K.BLUE, 96)
  codeBtn.addActionListener(_ => doGeneric(codeFld))
  codeFld.addActionListener(_ => doGeneric(codeFld))
  deck.add(wrap(codeFld, codeBtn), "code")

  // 4. Menu
  private val newBtn  = new RBtn("\uD83C\uDFB2 Nowa gra",     K.GREEN, 148)
  private val joinBtn = new RBtn("\uD83D\uDD17 Do\u0142\u0105cz",  K.BLUE,  148)
  newBtn.addActionListener (_  => { rawSend("1"); cl.show(deck, "chat") })
  joinBtn.addActionListener(_  => { rawSend("2"); cl.show(deck, "chat") })
  private val menuRow = new JPanel(new FlowLayout(FlowLayout.CENTER, 16, 12)) {
    setBackground(K.PANEL); add(newBtn); add(joinBtn)
  }
  deck.add(menuRow, "menu")

  // 5. Lobby
  private val startBtn = new RBtn("/start \u25ba", K.GREEN, 110)
  private val botBtn   = new RBtn("+ Bot \uD83E\uDD16", K.BLUE, 110)
  private val leaveBtn = new RBtn("Wyj\u015bd\u017a \u2716", K.RED, 110)
  startBtn.addActionListener(_ => rawSend("/start"))
  botBtn.addActionListener(_   => rawSend("/bot"))
  leaveBtn.addActionListener(_ => rawSend("/wyjdz"))
  private val lobbyChat = mkField()
  private val lobbyCht  = new RBtn("Chat \u25ba", new Color(50, 62, 78), 88)
  lobbyCht.addActionListener(_ => { val t = lobbyChat.getText.trim; if (t.nonEmpty) { rawSend(t); lobbyChat.setText("") } })
  lobbyChat.addActionListener(_ => { val t = lobbyChat.getText.trim; if (t.nonEmpty) { rawSend(t); lobbyChat.setText("") } })
  lobbyChat.setPreferredSize(new Dimension(170, 34))
  private val lobbyRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 9)) {
    setBackground(K.PANEL)
    setBorder(BorderFactory.createEmptyBorder(0, 6, 0, 6))
    add(startBtn); add(botBtn); add(leaveBtn)
    add(new JSeparator(SwingConstants.VERTICAL) { setPreferredSize(new Dimension(1, 30)); setForeground(K.BORDER) })
    add(lobbyChat); add(lobbyCht)
  }
  deck.add(lobbyRow, "lobby")

  // 6. Actions
  private val foldBtn  = new RBtn("Pas (F)",       K.RED,    106)
  private val checkBtn = new RBtn("Czekaj (CH)",   K.BLUE,   112)
  private val callBtn  = new RBtn("Wyrównaj",       K.GREEN,  106)
  private val raiseFld = new JTextField("0") {
    setFont(K.F_BODY); setBackground(K.INPUT_BG); setForeground(K.WHITE)
    setCaretColor(K.GOLD)
    setBorder(new CompoundBorder(new LineBorder(K.BORDER,1,true), BorderFactory.createEmptyBorder(5,8,5,8)))
    setPreferredSize(new Dimension(70, 34))
  }
  private val raiseBtn = new RBtn("Podbij (R)", K.YELLOW, 106)
  raiseBtn.setForeground(new Color(28, 22, 4))
  foldBtn.addActionListener(_  => act("f"))
  checkBtn.addActionListener(_ => act("ch"))
  callBtn.addActionListener(_  => act("c"))
  raiseBtn.addActionListener(_ => act("r " + raiseFld.getText.trim))
  raiseFld.addActionListener(_ => act("r " + raiseFld.getText.trim))
  private val actionRow = new JPanel(new FlowLayout(FlowLayout.CENTER, 8, 9)) {
    setBackground(K.PANEL)
    add(foldBtn); add(checkBtn); add(callBtn)
    add(new JSeparator(SwingConstants.VERTICAL) { setPreferredSize(new Dimension(1, 30)); setForeground(K.BORDER) })
    add(new JLabel("Kwota:") { setFont(K.F_SMALL); setForeground(K.SILVER) })
    add(raiseFld); add(raiseBtn)
  }
  deck.add(actionRow, "action")

  // ── Assemble ───────────────────────────────────────────────────────────────
  private val bottom = new JPanel(new BorderLayout()) {
    setBackground(K.PANEL)
    setBorder(new MatteBorder(1, 0, 0, 0, K.BORDER))
    add(promptBar, BorderLayout.NORTH)
    add(handPanel, BorderLayout.WEST)
    add(deck,      BorderLayout.CENTER)
  }
  add(bottom, BorderLayout.CENTER)

  // ── Public API ─────────────────────────────────────────────────────────────
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
    callBtn.setText(if (n > 0) s"Wyrównaj $n" else "Wyrównaj"))
  def syncMinRaise(n: Int): Unit   = SwingUtilities.invokeLater(() =>
    if (n > 0) raiseFld.setText(n.toString))

  // ── Private ────────────────────────────────────────────────────────────────
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
  private def mkField(): JTextField = new JTextField() {
    setFont(K.F_BODY); setBackground(K.INPUT_BG); setForeground(K.WHITE)
    setCaretColor(K.GOLD)
    setBorder(new CompoundBorder(new LineBorder(K.BORDER,1,true), BorderFactory.createEmptyBorder(6,10,6,10)))
    setPreferredSize(new Dimension(220, 34))
  }
  private def wrap(f: JTextField, b: JButton): JPanel = new JPanel(new BorderLayout(8, 0)) {
    setBackground(K.PANEL); setBorder(BorderFactory.createEmptyBorder(8, 12, 8, 12))
    add(f, BorderLayout.CENTER); add(b, BorderLayout.EAST)
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
      BorderFactory.createEmptyBorder(28, 36, 28, 36)
    ))
  }

  private def gbc(r: Int, c: Int, fill: Int = GridBagConstraints.HORIZONTAL,
                  span: Int = 1, anchor: Int = GridBagConstraints.CENTER): GridBagConstraints = {
    val g = new GridBagConstraints()
    g.gridx = c; g.gridy = r; g.fill = fill; g.gridwidth = span
    g.anchor = anchor; g.insets = new Insets(6, 6, 6, 6); g.weightx = 1.0; g
  }

  private def mkLbl(t: String, f: Font, fg: Color, align: Int = SwingConstants.LEFT): JLabel =
    new JLabel(t) { setFont(f); setForeground(fg); setHorizontalAlignment(align) }

  private def mkFld(d: String): JTextField = new JTextField(d) {
    setFont(K.F_BODY); setBackground(K.INPUT_BG); setForeground(K.WHITE); setCaretColor(K.GOLD)
    setBorder(new CompoundBorder(new LineBorder(K.BORDER,1,true), BorderFactory.createEmptyBorder(7,10,7,10)))
  }

  private val titleLbl  = mkLbl("\u2660  POKER ONLINE  \u2660", new Font("Georgia", Font.BOLD, 20), K.GOLD, SwingConstants.CENTER)
  private val subLbl    = mkLbl("Po\u0142\u0105cz si\u0119 z serwerem gry", K.F_SMALL, K.SILVER, SwingConstants.CENTER)
  private val hostField = mkFld("localhost")
  private val portField = mkFld("8080")
  private val statusLbl = mkLbl("", K.F_SMALL, K.RED, SwingConstants.CENTER)

  private val connectBtn = new RBtn("Po\u0142\u0105cz z serwerem", K.FELT, 200, 42) {
    setPreferredSize(new Dimension(200, 42))
  }

  inner.add(titleLbl,   gbc(0, 0, span = 2))
  inner.add(subLbl,     gbc(1, 0, span = 2))
  inner.add(mkLbl("Host:", K.F_BOLD, K.SILVER), gbc(2, 0, GridBagConstraints.NONE, anchor = GridBagConstraints.EAST))
  inner.add(hostField,  gbc(2, 1))
  inner.add(mkLbl("Port:", K.F_BOLD, K.SILVER), gbc(3, 0, GridBagConstraints.NONE, anchor = GridBagConstraints.EAST))
  inner.add(portField,  gbc(3, 1))
  inner.add(connectBtn, gbc(4, 0, GridBagConstraints.CENTER, 2))
  inner.add(statusLbl,  gbc(5, 0, span = 2))

  val outerC = new GridBagConstraints()
  outerC.fill = GridBagConstraints.NONE; outerC.anchor = GridBagConstraints.CENTER
  outerC.weightx = 1.0; outerC.weighty = 1.0
  add(inner, outerC)

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
      setStatus("Łączenie…", K.GOLD)
      onConnect(host, port)
    } catch { case _: NumberFormatException => setStatus("Nieprawidłowy port!", K.RED) }
  }
}

// ═══════════════════════════════════════════════════════════════════════════════
//  HEADER BAR
// ═══════════════════════════════════════════════════════════════════════════════
class HeaderBar extends JPanel(new BorderLayout()) {
  setBackground(new Color(12, 15, 20))
  setBorder(new CompoundBorder(
    new MatteBorder(0, 0, 1, 0, K.BORDER),
    BorderFactory.createEmptyBorder(10, 16, 10, 16)
  ))
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
    statusLbl.setText("\u25cf Roz\u0142\u0105czono"); statusLbl.setForeground(K.RED)
    sessionLbl.setVisible(false)
  })
  def setSession(code: String): Unit = SwingUtilities.invokeLater(() => {
    if (code.nonEmpty) { sessionLbl.setText(s"  $code  "); sessionLbl.setVisible(true) }
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
    frame.setPreferredSize(new Dimension(1040, 680))
    frame.setMinimumSize(new Dimension(780, 540))

    val st      = new State
    val table   = new TablePanel(st)
    val log     = new LogPanel
    val header  = new HeaderBar
    var sendFn: String => Unit = _ => ()

    val input = new InputPanel((msg: String) => sendFn(msg))

    val split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, table, log) {
      setDividerLocation(780); setResizeWeight(1.0)
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
      connectWS(
        host, port, st, table, log, input, header,
        onReady = (fn: String => Unit) => {
          sendFn = fn
          SwingUtilities.invokeLater(() => {
            rootLayout.show(root, "game")
            frame.setTitle(s"Poker Online – $host:$port")
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

    val rawSend: String => Unit = (text: String) => {
      queue.offer(TextMessage(s"INPUT:$text")); ()
    }

    val sink = Sink.foreach[Message] {
      case TextMessage.Strict(raw) if raw.startsWith("MSG:") =>
        val msg = raw.drop(4)
        log.addMsg(msg)
        Parser.process(msg, st)
        if (st.sessionCode.nonEmpty) header.setSession(st.sessionCode)
        st.localSeat.foreach { s => s.chips = st.myChips; s.hand = st.myHand }
        input.syncHand(st.myHand, st.myChips)
        input.syncCallAmount(st.callAmount)
        input.syncMinRaise(st.minRaise)
        if (st.inLobby) input.showLobby() else input.showGame()
        if (!st.inLobby) table.refresh()

      case TextMessage.Strict(raw) if raw.startsWith("PROMPT:") =>
        input.onPrompt(raw.drop(7))

      case _ =>
    }

    val flow = Flow.fromSinkAndSource(sink, source).watchTermination()(Keep.right)
    val (upgrade, closed) = Http().singleWebSocketRequest(
      WebSocketRequest(s"ws://$host:$port/poker"), flow)

    upgrade.foreach { resp =>
      if (resp.response.status.isSuccess()) {
        log.addMsg(s"*** Po\u0142\u0105czono z $host:$port ***")
        header.setOnline(host, port)
        onReady(rawSend)
      }
    }
    upgrade.failed.foreach(ex => onError(s"B\u0142\u0105d: ${ex.getMessage}"))
    closed.onComplete(_ => onClose())
  }
}