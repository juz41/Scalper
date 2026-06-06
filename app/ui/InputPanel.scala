package ui

import java.awt._
import javax.swing._
import javax.swing.border._

import network._

class InputPanel(rawSend: ClientToServer => Unit) extends JPanel(new BorderLayout()) {
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
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
      val cY = (getHeight - CardR.H) / 2
      if (_hand.nonEmpty)
        _hand.zipWithIndex.foreach { case (c, i) => CardR.drawCard(g2, 10 + i * (CardR.W + 8), cY, c) }
      else
        for (i <- 0 until 2) CardR.drawEmpty(g2, 10 + i * (CardR.W + 8), cY)
      g2.setFont(K.F_SBOLD); g2.setColor(K.GOLD)
      g2.drawString(s"${_chips} chips", 10, getHeight - 6)
    }
  }

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
  newBtn.addActionListener  (_ => { rawSend(ClientToServer.ChatInput("1")); cl.show(deck, "chat") })
  joinBtn.addActionListener (_ => { rawSend(ClientToServer.ChatInput("2")); cl.show(deck, "chat") })
  private val menuRow = new JPanel(new FlowLayout(FlowLayout.CENTER, 16, 12)) {
    setBackground(K.PANEL); add(newBtn); add(joinBtn)
  }
  deck.add(menuRow, "menu")

  // 5. Lobby
  private val startBtn = new RBtn("/start \u25ba",  K.GREEN, 112)
  private val botBtn   = new RBtn("+ Bot \uD83E\uDD16", K.BLUE,  112)
  private val leaveBtn = new RBtn("Leave \u2716",   K.RED,   112)
  startBtn.addActionListener(_ => rawSend(ClientToServer.LobbyCmd("/start")))
  botBtn.addActionListener(_   => rawSend(ClientToServer.LobbyCmd("/bot")))
  leaveBtn.addActionListener(_ => rawSend(ClientToServer.LobbyCmd("/leave")))
  private val lchat = mkField("Chat…"); lchat.setPreferredSize(new Dimension(170, 34))
  private val lsend = new RBtn("Chat \u25ba", new Color(48, 60, 76), 88)
  lsend.addActionListener(_ => { val t = lchat.getText.trim; if (t.nonEmpty) { rawSend(ClientToServer.ChatInput(t)); lchat.setText("") } })
  lchat.addActionListener(_ => { val t = lchat.getText.trim; if (t.nonEmpty) { rawSend(ClientToServer.ChatInput(t)); lchat.setText("") } })
  private val lobbyRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 9)) {
    setBackground(K.PANEL)
    setBorder(BorderFactory.createEmptyBorder(0, 6, 0, 6))
    add(startBtn); add(botBtn); add(leaveBtn)
    add(sep()); add(lchat); add(lsend)
  }
  deck.add(lobbyRow, "lobby")

  // 6. Game actions
  private val foldBtn  = new RBtn("Fold (F)",       K.RED,   106)
  private val checkCallBtn = new RBtn("Check (C)",  K.BLUE,  120)
  private val raiseFld = new JTextField("0") {
    setFont(K.F_BODY); setBackground(K.INPUT_BG); setForeground(K.WHITE); setCaretColor(K.GOLD)
    setBorder(new CompoundBorder(new LineBorder(K.BORDER, 1, true), BorderFactory.createEmptyBorder(5, 8, 5, 8)))
    setPreferredSize(new Dimension(72, 34))
  }
  private val raiseSlider = new JSlider(0, 1000, 0) {
    setBackground(K.PANEL); setForeground(K.GOLD)
    setPreferredSize(new Dimension(140, 34))
    setFocusable(false)
  }
  private val raiseBtn = new RBtn("Raise (R)", K.YELLOW, 106)
  raiseBtn.setForeground(new Color(26, 20, 4))

  private var _sliderSync = false
  raiseSlider.addChangeListener((_: javax.swing.event.ChangeEvent) => if (!_sliderSync) {
    _sliderSync = true; raiseFld.setText(raiseSlider.getValue.toString); _sliderSync = false
  })
  raiseFld.getDocument.addDocumentListener(new javax.swing.event.DocumentListener {
    private def sync(): Unit = if (!_sliderSync) {
      _sliderSync = true
      raiseFld.getText.trim.toIntOption.foreach(v => raiseSlider.setValue(v))
      _sliderSync = false
    }
    override def insertUpdate(e: javax.swing.event.DocumentEvent): Unit = sync()
    override def removeUpdate(e: javax.swing.event.DocumentEvent): Unit = sync()
    override def changedUpdate(e: javax.swing.event.DocumentEvent): Unit = sync()
  })

  foldBtn.addActionListener(_  => act(engine.Action.Fold))
  checkCallBtn.addActionListener(_ => act(engine.Action.Call))
  raiseBtn.addActionListener(_ => act(engine.Action.Raise(raiseFld.getText.trim.toIntOption.getOrElse(0))))
  raiseFld.addActionListener(_ => act(engine.Action.Raise(raiseFld.getText.trim.toIntOption.getOrElse(0))))

  private val actionRow = new JPanel(new FlowLayout(FlowLayout.CENTER, 8, 9)) {
    setBackground(K.PANEL)
    add(foldBtn); add(checkCallBtn); add(sep())
    add(new JLabel("Amount:") { setFont(K.F_SMALL); setForeground(K.SILVER) })
    add(raiseSlider); add(raiseFld); add(raiseBtn)
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
    val lo = p.toLowerCase
    if      (lo.contains("name"))     { cl.show(deck, "name");   nameFld.requestFocusInWindow() }
    else if (lo.contains("code"))     { cl.show(deck, "code");   codeFld.requestFocusInWindow() }
    else if (lo.contains("choose") || lo.contains("select") || lo.contains("menu")) { cl.show(deck, "menu") }
    else if (lo.contains("turn") || lo.contains("fold") || lo.contains("raise")) { cl.show(deck, "action"); raiseFld.requestFocusInWindow() }
    else                              { cl.show(deck, "chat"); chatFld.requestFocusInWindow() }
  })

  def showLobby(): Unit = SwingUtilities.invokeLater(() => { promptBar.setVisible(false); cl.show(deck, "lobby") })
  def showGame(): Unit = SwingUtilities.invokeLater(() => { cl.show(deck, "chat") })
  def hidePrompt(): Unit = SwingUtilities.invokeLater(() => promptBar.setVisible(false))

  def syncHand(hand: Seq[Card], chips: Int): Unit = SwingUtilities.invokeLater(() => { _hand = hand; _chips = chips; handPanel.repaint() })
  def syncCallAmount(n: Int): Unit = SwingUtilities.invokeLater(() => {
    if (n > 0) { checkCallBtn.setText(s"Call $n (C)"); checkCallBtn.setBackground(K.GREEN) }
    else       { checkCallBtn.setText("Check (C)"); checkCallBtn.setBackground(K.BLUE) }
  })
  def syncMinRaise(n: Int): Unit = SwingUtilities.invokeLater(() => if (n > 0) { raiseSlider.setMinimum(n); raiseFld.setText(n.toString) })
  def syncAvailableChips(n: Int): Unit = SwingUtilities.invokeLater(() => if (n > 0) raiseSlider.setMaximum(n))

  // ── Private helpers ───────────────────────────────────────────────────────
  private def act(cmd: engine.Action): Unit = {
    rawSend(ClientToServer.PlayerAct(cmd))
    promptBar.setVisible(false); cl.show(deck, "chat")
  }
  private def doGeneric(f: JTextField): Unit = {
    val t = f.getText.trim; if (t.nonEmpty) rawSend(ClientToServer.ChatInput(t)); f.setText("")
    promptBar.setVisible(false); cl.show(deck, "chat")
  }
  private def doChat(): Unit = {
    val t = chatFld.getText.trim; if (t.nonEmpty) { rawSend(ClientToServer.ChatInput(t)); chatFld.setText("") }
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