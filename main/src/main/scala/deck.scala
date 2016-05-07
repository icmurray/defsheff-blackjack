package defsheff

import scalaz.NonEmptyList
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.traverse._

import scalaz.concurrent._

import data._

object deck {

  def shuffleDeck(d: Deck): Task[Deck] = Task.now {
    scala.util.Random.shuffle(d)
  }

  val standardDeck = for {
    suit <- List(Clubs, Diamonds, Hearts, Spades)
    rank <- (2 to 10).map(NC) ++ List(Jack, Queen, King, Ace)
  } yield Card(rank, suit)

  /* Deal `handSize` items to the given number of players.  Return the undealt
   * items too.  It's at this point, we ensure that the returned Hands are
   * non-empty.  In the case that there aren't enough cards left in the deck,
   * None is returned.
   */
  def deal(numHands: Int, handSize: Int)(deck: Deck): Option[(List[Hand], Deck)] = {
    require(numHands > 0 && handSize > 0, "Dealing requires at least 1 hand of at least 1 card")
    val numCards = numHands * handSize
    deck.take(numCards) match {
      case toDeal if (toDeal.length < numCards) => None
      case toDeal =>
        SeqUtils.partitionByDealing((numHands, toDeal))
          .flatMap(hands => hands.map(toNel).sequence)
          .map(hands => (hands, deck.drop(numCards)))
    }
  }

  private def toNel[A](xs: List[A]): Option[NonEmptyList[A]] = xs match {
    case Nil       => None
    case x :: tail => Some(NonEmptyList(x, tail:_*))
  }
}
