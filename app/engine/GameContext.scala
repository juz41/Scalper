package engine

import model.{Card, Hand}

/**
 * A single recorded action in the hand history.
 *
 * @param playerName  Name of the acting player.
 * @param street      Which street this happened on.
 * @param action      What the player did.
 * @param potAfter    Pot size after the action was resolved.
 */
case class ActionRecord(
  playerName: String,
  street:     Street,
  action:     Action,
  potAfter:   Int
)

/** The current betting street. */
sealed trait Street
object Street {
  case object PreFlop extends Street { override def toString = "Pre-flop" }
  case object Flop    extends Street { override def toString = "Flop"     }
  case object Turn    extends Street { override def toString = "Turn"     }
  case object River   extends Street { override def toString = "River"    }
}

/**
 * Everything the AI is allowed to observe when it must make a decision.
 *
 * Deliberately immutable: construct a fresh snapshot before each AI call;
 * the AI must not be able to mutate game state.
 *
 * @param street           Current street.
 * @param community        Visible community cards (0, 3, 4, or 5 cards).
 * @param holeCards        The AI player's own hole cards.
 * @param pot              Current pot size (before this action).
 * @param toCall           Extra chips this player must put in to stay in.
 * @param availableChips   This player's remaining chips.
 * @param currentBet       The highest total bet on this street so far.
 * @param numActivePlayers How many players have not yet folded.
 * @param opponentStacks   Chip counts of every other active (non-folded) player: (name -> chips).
 * @param position         The acting player's position (e.g., "SB", "BB", "BTN", "UTG").
 * @param opponentPositions Mapping of opponent names to their positions.
 * @param history          Ordered list of all actions taken this hand.
 */
case class GameContext(
  street:            Street,
  community:         List[Card],
  holeCards:         Hand,
  pot:               Int,
  toCall:            Int,
  availableChips:    Int,
  currentBet:        Int,
  numActivePlayers:  Int,
  opponentStacks:    List[(String, Int)],
  position:          String,
  opponentPositions: Map[String, String],
  history:           List[ActionRecord]
) {

  /** Total money committed to this hand across all streets. */
  def totalPotCommitment: Int = pot + toCall

  /** Actions taken by any player on the current street. */
  def currentStreetHistory: List[ActionRecord] = history.filter(_.street == street)

  /** How many raises have happened on this street (useful for aggression detection). */
  def raisesThisStreet: Int = currentStreetHistory.count(_.action.isInstanceOf[Action.Raise])

  /** True if at least one opponent has raised on the current street. */
  def facingAggression: Boolean = raisesThisStreet > 0

  /** Pot-odds ratio: fraction of the final pot we'd be paying to call.
   * Returns 0.0 if toCall is 0 (free check).
   */
  def potOdds: Double =
    if (toCall == 0) 0.0 else toCall.toDouble / (pot + toCall).toDouble

  /** Chips held by the shortest-stacked opponent (useful for shove/fold decisions). */
  def shortestOpponentStack: Option[Int] =
    if (opponentStacks.isEmpty) None else Some(opponentStacks.map(_._2).min)
}
