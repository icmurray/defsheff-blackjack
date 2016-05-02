package defsheff

import scalaz._
import scalaz.concurrent._
import Scalaz._

import data._

object blackjack {

  sealed trait Outcome { def value: Int }
  case class Score(value: Int) extends Outcome
  case class Bust(value: Int) extends Outcome
  case object Blackjack  extends Outcome { def value = 21 }

  case class Table(hands: List[Hand], deck: Deck) {
    def dealer = hands.last
    def players = hands.init
  }

  val standardDeck = for {
    suit <- List(Clubs, Diamonds, Hearts, Spades)
    rank <- (2 to 10).map(NC) ++ List(Jack, Queen, King, Ace)
  } yield Card(rank, suit)

  def shuffleDeck(d: Deck): Task[Deck] = Task.now {
    scala.util.Random.shuffle(d)
  }

  def initialDeal[A](numPlayers: Int)(deck: List[A]): (List[List[A]], List[A]) = {
    val numHands = numPlayers+1
    val remainingDeck = deck.drop(numHands*2)
    val dealt = deck.sliding(numHands, numHands).take(2).toList.transpose
    (dealt, remainingDeck)
  }

  def initialTable(numPlayers: Int)(deck: Deck) = {
    val (hands, remaining) = initialDeal[Card](numPlayers)(deck)
    Table(hands, remaining)
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
      case v if (isBlackJack(hand)) => Blackjack
      case v if (v > 21)  => Bust(v)
      case v if (v <= 21) => Score(v)
    }
  }

  def isBlackJack(hand: Hand) = handValue(hand) == 21 && hand.length == 2

}
