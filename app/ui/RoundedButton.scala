package ui

import java.awt._
import javax.swing._

import network._

class RBtn(label: String, initialBg: Color, w: Int = 0, h: Int = 34) extends JButton(label) {
  setFont(K.F_BOLD); setForeground(K.WHITE); setBackground(initialBg)
  setFocusPainted(false); setBorderPainted(false); setOpaque(true)
  setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR))
  if (w > 0) setPreferredSize(new Dimension(w, h))
  override def paintComponent(g: Graphics): Unit = {
    val g2 = g.asInstanceOf[Graphics2D]
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    val currentBg = getBackground
    g2.setColor(if (getModel.isRollover) currentBg.brighter() else currentBg)
    g2.fillRoundRect(0, 0, getWidth, getHeight, 9, 9)
    super.paintComponent(g)
  }
}
