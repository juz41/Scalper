package ui

import java.awt._
import javax.swing._
import javax.swing.border.MatteBorder

import network._

class LogPanel extends JPanel(new BorderLayout()) {
  setBackground(K.PANEL)
  setPreferredSize(new Dimension(320, 0))
  setBorder(new MatteBorder(0, 1, 0, 0, K.BORDER))

  private val msgContainer = new JPanel()
  msgContainer.setLayout(new BoxLayout(msgContainer, BoxLayout.Y_AXIS))
  msgContainer.setBackground(K.PANEL)

  private val scroll = new JScrollPane(msgContainer) {
    setBorder(BorderFactory.createEmptyBorder())
    setBackground(K.PANEL)
    getViewport.setBackground(K.PANEL)
    setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
    getVerticalScrollBar.setUnitIncrement(16)
  }

  private val hdr = new JLabel("  \u2663  GAME LOG") {
    setFont(K.F_SBOLD); setForeground(K.GOLD); setOpaque(true)
    setBackground(new Color(18, 24, 34))
    setBorder(BorderFactory.createEmptyBorder(9, 8, 9, 8))
  }
  add(hdr, BorderLayout.NORTH); add(scroll, BorderLayout.CENTER)

  def addMsg(msg: String): Unit = SwingUtilities.invokeLater(() => {
    val isChat = msg.startsWith("[Chat]")
    val isSys = msg.startsWith("***") || msg.startsWith("===") ||
      msg.startsWith("[!]") || msg.startsWith("---") ||
      msg.toLowerCase.contains("wins") ||
      msg.toLowerCase.contains("winner") ||
      msg.toLowerCase.contains("joined") ||
      msg.toLowerCase.contains("left")

    val (align, shapeType, bgCol, fgCol) =
      if (isChat) {
        (FlowLayout.LEFT, "chat", new Color(42, 60, 90), K.WHITE)
      } else if (isSys) {
        (FlowLayout.CENTER, "system", new Color(75, 35, 35), K.GOLD)
      } else {
        (FlowLayout.RIGHT, "game", new Color(30, 36, 46), K.SILVER)
      }

    val text = if (isChat) msg.replaceFirst("\\[Chat\\]\\s*", "") else msg

    val bubble = new MessageBubble(text, shapeType, bgCol, fgCol)
    val wrap = new JPanel(new FlowLayout(align, 8, 4))
    wrap.setBackground(K.PANEL)
    wrap.add(bubble)

    msgContainer.add(wrap)
    msgContainer.revalidate()

    SwingUtilities.invokeLater(() => {
      val bar = scroll.getVerticalScrollBar
      bar.setValue(bar.getMaximum)
    })

    if (msgContainer.getComponentCount > 150) {
      msgContainer.remove(0)
    }
  })
}
