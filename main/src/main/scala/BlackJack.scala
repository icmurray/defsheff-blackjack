package defsheff

import scalaz._
import scalaz.concurrent._
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

  sealed trait Outcome { def value: Int }
  case class Score(value: Int) extends Outcome
  case class Bust(value: Int) extends Outcome
  case object BJ  extends Outcome { def value = 21 }

  case class Card(rank: Rank, suit: Suit)
  type Hand = List[Card]
  type Deck = List[Card]

  val standardDeck = for {
    suit <- List(Clubs, Diamonds, Hearts, Spades)
    rank <- (2 to 10).map(NC) ++ List(Jack, Queen, King, Ace)
  } yield Card(rank, suit)

  def shuffleDeck(d: Deck): Task[Deck] = Task.now {
    scala.util.Random.shuffle(d)
  }

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

  def handOutcome(hand: Hand): Outcome = {
    handValue(hand) match {
      case v if (isBlackJack(hand)) => BJ
      case v if (v > 21)  => Bust(v)
      case v if (v <= 21) => Score(v)
    }
  }

  def isBlackJack(hand: Hand) = handValue(hand) == 21 && hand.length == 2

}
