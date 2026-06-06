package ui

import java.awt._
import javax.swing._

import network._

class MessageBubble(text: String, shape: String, bgCol: Color, fgCol: Color) extends JPanel(new BorderLayout()) {
  setOpaque(false)

  val area = new JTextArea(text) {
    setOpaque(false)
    setEditable(false)
    setLineWrap(true)
    setWrapStyleWord(true)
    setFont(K.F_BODY)
    setForeground(fgCol)

    if (shape == "system") {
      setAlignmentX(0.5f)
    }

    override def getPreferredSize: Dimension = {
      val base = super.getPreferredSize
      val maxWidth = 300
      if (base.width > maxWidth) {
        setSize(new Dimension(maxWidth, Short.MaxValue))
        new Dimension(maxWidth, super.getPreferredSize.height)
      } else base
    }
  }

  if (shape == "system") {
    val centerPanel = new JPanel()
    centerPanel.setLayout(new BoxLayout(centerPanel, BoxLayout.X_AXIS))
    centerPanel.setOpaque(false)
    centerPanel.add(Box.createHorizontalGlue())
    centerPanel.add(area)
    centerPanel.add(Box.createHorizontalGlue())
    add(centerPanel, BorderLayout.CENTER)
  } else {
    add(area, BorderLayout.CENTER)
  }

  setBorder(BorderFactory.createEmptyBorder(6, 10, 6, 10))

  override def paintComponent(g: Graphics): Unit = {
    val g2 = g.asInstanceOf[Graphics2D]
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2.setColor(bgCol)

    shape match {
      case "chat" =>
        g2.fillRoundRect(0, 0, getWidth, getHeight, 16, 16)

      case "system" =>
        g2.fillRect(0, 0, getWidth, getHeight)
        g2.setColor(new Color(255, 255, 255, 30))
        g2.drawRect(0, 0, getWidth - 1, getHeight - 1)

      case "game" =>
        val arc = 12
        g2.fillRoundRect(0, 0, getWidth, getHeight, arc, arc)
        g2.setColor(new Color(255, 255, 255, 15))
        g2.drawRoundRect(0, 0, getWidth - 1, getHeight - 1, arc, arc)
    }
    super.paintComponent(g)
  }
}
