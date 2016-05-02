package defsheff

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

  /* Deal `handSize` items to the given number of players.  Return the undealt items too. */
  def deal(numHands: Int, handSize: Int)(deck: Deck): Option[(List[Hand], Deck)] = {
    val numCards = numHands * handSize
    SeqUtils.partitionByDealing((numHands, deck.take(numCards)))
      .map(dealt => (dealt, deck.drop(numCards)))
  }
}
