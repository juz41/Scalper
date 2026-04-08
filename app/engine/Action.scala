package engine

/** All decisions a player can make during a betting round. */
sealed trait Action
object Action {
  case object Fold              extends Action
  case object Call              extends Action
  case class  Raise(amount: Int) extends Action
}
