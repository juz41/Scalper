package ui

import java.awt._
import javax.swing._
import javax.swing.border._

import network._

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
