package ui

import network.{Card, K}

import java.awt.{BasicStroke, Color, Font, Graphics2D}

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
    g.setColor(new Color(255, 255, 255, 80))
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