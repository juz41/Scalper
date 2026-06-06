package network

import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import org.apache.pekko.stream.OverflowStrategy
import org.apache.pekko.stream.scaladsl.{Flow, Keep, Sink, Source}

import java.awt._
import javax.swing._

import scala.collection.immutable.List

import upickle.default._

import ui._

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
  var currentStreetBet: Int = 0
  var inLobby:     Boolean = true
  var localName:   String  = ""
  var lastJoinedName: String = ""

  def localSeat: Option[Seat] = seats.find(_.isLocal)

  def streetReset(): Unit = {
    currentStreetBet = 0; seats.foreach(_.bet = 0)
  }

  def fullReset(): Unit = {
    community = Nil; pot = 0; myHand = Nil; callAmount = 0; minRaise = 0; currentStreetBet = 0
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
    var sendFn: ClientToServer => Unit = _ => ()

    val input = new InputPanel((msg: ClientToServer) => sendFn(msg))

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
        onReady = (fn: ClientToServer => Unit) => {
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
                         onReady: (ClientToServer => Unit) => Unit,
                         onError: String => Unit,
                         onClose: () => Unit
                       ): Unit = {
    implicit val sys: ActorSystem[Nothing] = system
    import sys.executionContext

    var awaitingAction = false

    val (queue, source) =
      Source.queue[Message](256, OverflowStrategy.dropTail).preMaterialize()

    val rawSend: ClientToServer => Unit = (msg: ClientToServer) => {
      awaitingAction = false
      queue.offer(TextMessage(write(msg)))
      ()
    }

    val sink = Sink.foreach[Message] {
      case TextMessage.Strict(raw) =>
        try {
          val serverMsg = read[ServerToClient](raw)

          def toUICards(cards: List[model.Card]): Seq[Card] = cards.map { c =>
            val suitSymbol = c.suit match {
              case model.Suit.Hearts   => "\u2665"
              case model.Suit.Diamonds => "\u2666"
              case model.Suit.Clubs    => "\u2663"
              case model.Suit.Spades   => "\u2660"
            }
            Card(c.rank.name, suitSymbol)
          }

          serverMsg match {
            case ServerToClient.GameLog(msg, isChat) =>
              log.addMsg(if(isChat) s"[Chat] $msg" else msg)

            case ServerToClient.SyncState(pot, round, comm, bet) =>
              st.pot = pot
              st.round = round
              st.community = toUICards(comm)
              st.currentStreetBet = bet
              if (round == "Pre-Flop") {
                st.seats.foreach(s => { s.bet = 0; s.folded = false })
                st.community = Nil
              }

            case ServerToClient.SyncPlayer(name, chips, folded, bet, active) =>
              st.addPlayer(name)
              st.seats.find(_.name == name).foreach { s =>
                if (chips >= 0) s.chips = chips
                s.folded = folded
                if (bet >= 0) s.bet = bet
                s.active = active
                if (folded) { s.backCards = 0; s.hand = Nil }
              }
              if (name == st.localName && chips >= 0) st.myChips = chips

            case ServerToClient.HoleCards(cards) =>
              val uiCards = toUICards(cards)
              st.myHand = uiCards
              st.localSeat.foreach { s => s.hand = uiCards; s.backCards = 0 }

            case ServerToClient.ShowdownCards(name, cards) =>
              st.seats.find(_.name == name).foreach { s => s.hand = toUICards(cards); s.backCards = 0 }

            case ServerToClient.PlayerJoined(name) =>
              st.addPlayer(name)
              log.addMsg(s"*** $name joined room ***")
              if (st.localName.isEmpty) {
                st.localName = name
                st.seats.find(_.name == name).foreach { s => s.isLocal = true; s.backCards = 0 }
              }

            case ServerToClient.PlayerLeft(name) =>
              st.removePlayer(name)
              log.addMsg(s"*** $name left the room ***")

            case ServerToClient.SessionInfo(code) =>
              st.sessionCode = code
              st.inLobby = true
              header.setSession(code)

            case ServerToClient.LobbyState(inLobby) =>
              st.inLobby = inLobby

            case ServerToClient.PromptAction(callAmount, minRaise, chips) =>
              st.callAmount = callAmount
              st.minRaise = minRaise
              input.syncCallAmount(callAmount)
              input.syncMinRaise(minRaise)
              input.syncAvailableChips(chips)
              awaitingAction = true
              input.onPrompt("Your turn! Fold, Call, or Raise?")

            case ServerToClient.PromptMenu(text) =>
              input.onPrompt(text)
          }

          input.syncHand(st.myHand, st.myChips)
          if (st.sessionCode.nonEmpty) {
            if (st.inLobby)
              input.showLobby()
            else if (!awaitingAction)
              input.showGame()
          }
          table.refresh()

        } catch { case e: Exception => println(s"Failed to decode WS JSON: ${e.getMessage}") }
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