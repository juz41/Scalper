package engine

import scala.util.Random

/**
 * Heuristic-based AI decision engine.
 *
 * Decisions are made from a [[GameContext]] snapshot which includes the full
 * hand action-history, community cards, pot-odds, and aggression signals —
 * rather than just the raw call amount.
 *
 * Strategy summary
 * ─────────────────
 *  • Facing aggression (someone has raised this street):
 *      – Tighter fold threshold; calls only if pot-odds are reasonable
 *  • Free check available:
 *      – Always checks (never folds for free)
 *  • Post-flop (Flop / Turn / River):
 *      – More likely to raise, less random; scales raise size with pot
 *  • Pre-flop:
 *      – Standard random strategy weighted by stack pressure
 *
 * Raise semantics: Raise(total) means "my total contribution this street
 * should be `total` chips", which is what [[ConsoleGame.bettingRound]] expects.
 */
object ComputerAI {

  private val rng = new Random()

  /**
   * Choose an action given the complete observable game state.
   *
   * @param ctx A [[GameContext]] snapshot built just before this player must act.
   * @return    An [[Action]]; [[Action.Raise]](total) expresses a total street bet.
   */
  def decideAction(ctx: GameContext): Action = {
    // Free check is always taken – never fold for free
    if (ctx.toCall == 0 && !ctx.facingAggression) {
      return if (shouldBluff(ctx)) buildRaise(ctx) else Action.Call
    }

    // If pot-odds are terrible and facing real aggression, fold more often
    if (ctx.facingAggression && ctx.potOdds > 0.45 && ctx.raisesThisStreet >= 2) {
      if (rng.nextInt(10) < 5) return Action.Fold
    }

    // Stack pressure: if we'd be putting in more than 40 % of our remaining chips
    // just to call, fold more aggressively
    val callFraction =
      if (ctx.availableChips == 0) 1.0
      else ctx.toCall.toDouble / ctx.availableChips

    if (callFraction > 0.4 && rng.nextInt(10) < 3) return Action.Fold
    if (callFraction > 0.7 && rng.nextInt(10) < 6) return Action.Fold

    // Roll for call vs. raise
    val raiseChance = ctx.street match {
      case Street.PreFlop => 3   // 30 % pre-flop raise
      case Street.Flop    => 4   // 40 %
      case Street.Turn    => 4   // 40 %
      case Street.River   => 5   // 50 % – more aggressive on the river
    }

    if (rng.nextInt(10) < raiseChance && ctx.availableChips > ctx.toCall)
      buildRaise(ctx)
    else
      Action.Call
  }

  // Build a raise action using pot-sized raise logic, capped at available chips
  private def buildRaise(ctx: GameContext): Action = {
    val minRaise  = ctx.toCall + 1                        // must beat the current bet
    val potRaise  = ctx.toCall + ctx.pot                  // pot-sized raise
    val extraJitter = (rng.nextInt(5)) * 10               // 0 / 10 / 20 / 30 / 40 variance
    val desired   = (potRaise + extraJitter).max(minRaise)
    val total     = desired.min(ctx.availableChips)
    if (total <= ctx.toCall) Action.Call else Action.Raise(total)
  }

  // Small chance of bluffing (raising) even when checking is free
  private def shouldBluff(ctx: GameContext): Boolean =
    rng.nextInt(10) < 2   // 20 % bluff frequency on free streets
}
