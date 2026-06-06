package ui

import java.awt._
import javax.swing._

import network._

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
    if (s.isLocal) return // Local player's cards are shown in the bottom left corner

    if (s.hand.nonEmpty) {
      // Show face-up cards (opponent at showdown)
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