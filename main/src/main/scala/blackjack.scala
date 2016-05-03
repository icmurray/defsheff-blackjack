package defsheff

import scalaz._
import Scalaz._

import data._

object blackjack {

  sealed trait Outcome { def value: Int }
  final case class Score(value: Int) extends Outcome
  final case class Bust(value: Int) extends Outcome
  final case object Blackjack  extends Outcome { def value = 21 }

  final case class Table(hands: List[Hand], deck: Deck) {
    def dealer = hands.last
    def players = hands.init
  }

  def blackjackDeal(numPlayers: Int) = deck.deal(numPlayers+1, 2) _

  def initialTable(numPlayers: Int)(deck: Deck) = {
    blackjackDeal(numPlayers)(deck).
      map((Table.apply _).tupled)
  }

  def cardValue(c: Card) = c.rank match {
    case NC(r)     => NonEmptyList(r)
    case Ace       => NonEmptyList(1, 11)
    case otherwise => NonEmptyList(10)
  }

  def handValue(hand: Hand): Int = {
    hand.map(cardValue).sequence.map(_.sumr).partition(_ <= 21) match {
      case (Nil,  bust) => bust.min
      case (legal, _)   => legal.max
    }
  }

  def handOutcome(hand: Hand): Outcome = {
    handValue(hand) match {
      case v if (isBlackJack(hand)) => Blackjack
      case v if (v > 21)  => Bust(v)
      case v if (v <= 21) => Score(v)
    }
  }

  def isBlackJack(hand: Hand) = handValue(hand) == 21 && hand.length == 2

  private implicit class NelOps[A](nel: NonEmptyList[A]) {
    def partition(pred: A => Boolean): (List[A], List[A]) = nel.list.toList.partition(pred)
  }
}
