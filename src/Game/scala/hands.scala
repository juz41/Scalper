package Game

sealed trait HandRank { val tier: Int; val label: String }
case object HighCard      extends HandRank { val tier = 1;  val label = "High Card"       }
case object OnePair       extends HandRank { val tier = 2;  val label = "One Pair"        }
case object TwoPair       extends HandRank { val tier = 3;  val label = "Two Pair"        }
case object ThreeOfAKind  extends HandRank { val tier = 4;  val label = "Three of a Kind" }
case object Straight      extends HandRank { val tier = 5;  val label = "Straight"        }
case object Flush         extends HandRank { val tier = 6;  val label = "Flush"           }
case object FullHouse     extends HandRank { val tier = 7;  val label = "Full House"      }
case object FourOfAKind   extends HandRank { val tier = 8;  val label = "Four of a Kind"  }
case object StraightFlush extends HandRank { val tier = 9;  val label = "Straight Flush"  }
case object RoyalFlush    extends HandRank { val tier = 10; val label = "Royal Flush"     }

case class EvaluatedHand(rank: HandRank, tiebreakers: List[Int]) extends Ordered[EvaluatedHand] {
  def compare(that: EvaluatedHand): Int = {
    val tierCmp = this.rank.tier.compare(that.rank.tier)
    if (tierCmp != 0) tierCmp
    else this.tiebreakers
      .zip(that.tiebreakers)
      .map { case (a, b) => a.compare(b) }
      .find(_ != 0)
      .getOrElse(0)
  }
  override def toString: String = rank.label
}

private[Game] object CoreEvaluator {

  def evaluate5(cards: List[Card], cfg: DeckConfig): EvaluatedHand = {
    require(cards.length == 5, s"evaluate5 needs exactly 5 cards, got ${cards.length}")

    val sortedVals = cards.map(_.rank).sorted.reverse
    val isFlush    = cards.map(_.suit).distinct.size == 1
    val isStraight = isStraightSeq(sortedVals, cfg)
    val grouped    = cards.groupBy(_.rank).toList
                       .sortBy { case (v, cs) => (cs.size, v) }
                       .reverse
    val counts     = grouped.map(_._2.size)
    val tb         = grouped.flatMap { case (v, cs) => List.fill(cs.size)(v) }

    (isFlush, isStraight, counts) match {
      case (true, true, _) if sortedVals.head == cfg.resolvedHighRank => EvaluatedHand(RoyalFlush,    tb)
      case (true, true, _)                                             => EvaluatedHand(StraightFlush, tb)
      case (_, _, List(4, 1))                                          => EvaluatedHand(FourOfAKind,   tb)
      case (_, _, List(3, 2))                                          => EvaluatedHand(FullHouse,     tb)
      case (true, _, _)                                                => EvaluatedHand(Flush,         tb)
      case (_, true, _)                                                => EvaluatedHand(Straight,      tb)
      case (_, _, List(3, 1, 1))                                       => EvaluatedHand(ThreeOfAKind,  tb)
      case (_, _, List(2, 2, 1))                                       => EvaluatedHand(TwoPair,       tb)
      case (_, _, List(2, 1, 1, 1))                                    => EvaluatedHand(OnePair,       tb)
      case _                                                           => EvaluatedHand(HighCard,      tb)
    }
  }

  private def isStraightSeq(sortedVals: List[Int], cfg: DeckConfig): Boolean = {
    val normal = sortedVals.zip(sortedVals.tail).forall { case (a, b) => a - b == 1 }
    val wheel  =
      sortedVals.head == cfg.resolvedHighRank &&
      sortedVals.tail == (cfg.lowRank until cfg.lowRank + 4).toList.sorted.reverse
    normal || wheel
  }
}
