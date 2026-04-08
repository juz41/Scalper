package engine

import scala.util.Random

/**
 * Random decision-making for the computer player.
 *
 * Fold probability  : 20 %
 * Call probability  : 50 %
 * Raise probability : 30 %
 *
 * Why random for Phase 1: a strategy-based AI can be dropped in later
 * by replacing this object without touching any other code.
 */
object ComputerAI {

  private val rng = new Random()

  /** Chooses an action given the current bet to call and available chips. */
  def decideAction(toCall: Int, availableChips: Int): Action = {
    val roll = rng.nextInt(10)
    roll match {
      case n if n < 2 => Action.Fold
      case n if n < 7 => Action.Call
      case _ =>
        val extra = (rng.nextInt(5) + 1) * 10     // 10 / 20 / 30 / 40 / 50
        val raise = (toCall + extra).min(availableChips)
        Action.Raise(raise)
    }
  }
}
