package defsheff

import scalaz._
import Scalaz._

object BlackJack {

  sealed trait Suit
  case object Clubs extends Suit
  case object Diamonds extends Suit
  case object Hearts extends Suit
  case object Spades extends Suit

  sealed trait Rank
  case class NC(r: Int) extends Rank { assert ((2 <= r) && (r <= 10)) }
  case object Jack extends Rank
  case object Queen extends Rank
  case object King extends Rank
  case object Ace extends Rank

  case class Card(rank: Rank, suit: Suit)
  type Hand = List[Card]

  def cardValue(c: Card) = c.rank match {
    case NC(r)     => List(r)
    case Ace       => List(1, 11)
    case otherwise => List(10)
  }

  def handValue(hand: Hand): Int =
    hand.map(cardValue).sequence.map(_.sum).partition(_ <= 21) match {
      case (Nil, Nil)  => 0   // ????
      case (Nil, bust) => bust.min
      case (legal, _)  => legal.max
    }

}
