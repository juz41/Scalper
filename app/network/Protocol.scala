package network

import engine.Action
import model.{Card, Rank, Suit}
import upickle.default._

object Protocol {
  // Ręczne serializatory dla domenowych typów gry, by nie brudzić oryginalnego modelu
  implicit val rwAction: ReadWriter[Action] = readwriter[ujson.Value].bimap[Action](
    {
      case Action.Fold       => ujson.Obj("$type" -> "Fold")
      case Action.Call       => ujson.Obj("$type" -> "Call")
      case Action.Raise(amt) => ujson.Obj("$type" -> "Raise", "amount" -> amt)
    },
    json => json("$type").str match {
      case "Fold"  => Action.Fold
      case "Call"  => Action.Call
      case "Raise" => Action.Raise(json("amount").num.toInt)
    }
  )

  implicit val rwCard: ReadWriter[Card] = readwriter[String].bimap[Card](
    c => s"${c.rank.name}${c.suit.symbol}",
    str => {
      val rank = Rank.all.find(_.name == str.dropRight(1)).get
      val suit = Suit.all.find(_.symbol == str.takeRight(1)).get
      Card(rank, suit)
    }
  )
}

// Wiadomości wysyłane od Serwera do Klienta
sealed trait ServerToClient
object ServerToClient {
  import Protocol._

  case class GameLog(msg: String, isChat: Boolean = false) extends ServerToClient
  case class SyncState(pot: Int, round: String, community: List[Card], currentStreetBet: Int) extends ServerToClient
  case class SyncPlayer(name: String, chips: Int, folded: Boolean, bet: Int, active: Boolean) extends ServerToClient
  case class HoleCards(cards: List[Card]) extends ServerToClient
  case class ShowdownCards(name: String, cards: List[Card]) extends ServerToClient
  case class PromptAction(callAmount: Int, minRaise: Int, availableChips: Int) extends ServerToClient
  case class PromptMenu(text: String) extends ServerToClient
  case class PlayerLeft(name: String) extends ServerToClient
  case class PlayerJoined(name: String) extends ServerToClient
  case class SessionInfo(code: String) extends ServerToClient
  case class LobbyState(inLobby: Boolean) extends ServerToClient

  implicit val rwGameLog: ReadWriter[GameLog] = macroRW
  implicit val rwSyncState: ReadWriter[SyncState] = macroRW
  implicit val rwSyncPlayer: ReadWriter[SyncPlayer] = macroRW
  implicit val rwHoleCards: ReadWriter[HoleCards] = macroRW
  implicit val rwShowdownCards: ReadWriter[ShowdownCards] = macroRW
  implicit val rwPromptAction: ReadWriter[PromptAction] = macroRW
  implicit val rwPromptMenu: ReadWriter[PromptMenu] = macroRW
  implicit val rwPlayerLeft: ReadWriter[PlayerLeft] = macroRW
  implicit val rwPlayerJoined: ReadWriter[PlayerJoined] = macroRW
  implicit val rwSessionInfo: ReadWriter[SessionInfo] = macroRW
  implicit val rwLobbyState: ReadWriter[LobbyState] = macroRW
  implicit val rw: ReadWriter[ServerToClient] = macroRW
}

// Wiadomości wysyłane od Klienta do Serwera
sealed trait ClientToServer
object ClientToServer {
  import Protocol._

  case class ChatInput(text: String) extends ClientToServer
  case class LobbyCmd(cmd: String) extends ClientToServer
  case class PlayerAct(action: Action) extends ClientToServer

  implicit val rwChatInput: ReadWriter[ChatInput] = macroRW
  implicit val rwLobbyCmd: ReadWriter[LobbyCmd] = macroRW
  implicit val rwPlayerAct: ReadWriter[PlayerAct] = macroRW
  implicit val rw: ReadWriter[ClientToServer] = macroRW
}