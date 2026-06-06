package ui

import java.awt._
import javax.swing._
import javax.swing.border._

import network._

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
  private val portField = fld("2137")
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
