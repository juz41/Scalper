package Game

sealed trait PlayerAction
case object Fold                extends PlayerAction
case object Check               extends PlayerAction
case class Call(amount: Int)    extends PlayerAction
case class Raise(amount: Int)   extends PlayerAction
case class AllIn(amount: Int)   extends PlayerAction

class Player(val name: String, var chips: Int) {
  var holeCards:  List[Card] = Nil
  var currentBet: Int        = 0
  var isFolded:   Boolean    = false
  var isAllIn:    Boolean    = false

  def receiveCard(card: Card): Unit =
    holeCards = holeCards :+ card

  def placeBet(amount: Int): Int = {
    val actual = math.min(amount, chips)
    chips      -= actual
    currentBet += actual
    actual
  }

  def resetForNewRound(): Unit = {
    holeCards  = Nil
    currentBet = 0
    isFolded   = false
    isAllIn    = false
  }

  def canAct: Boolean = !isFolded && !isAllIn && chips > 0

  override def toString: String =
    s"$name [chips: $chips${if (isFolded) ", FOLDED" else if (isAllIn) ", ALL-IN" else ""}]"
}
