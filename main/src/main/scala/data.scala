package defsheff

import scalaz.NonEmptyList

package data {

  final case class Card(rank: Rank, suit: Suit)

  sealed trait Suit
  final case object Clubs extends Suit
  final case object Diamonds extends Suit
  final case object Hearts extends Suit
  final case object Spades extends Suit

  sealed trait Rank
  final case class NC(r: Int) extends Rank { assert ((2 <= r) && (r <= 10)) }
  final case object Jack extends Rank
  final case object Queen extends Rank
  final case object King extends Rank
  final case object Ace extends Rank

}

/* Type aliases that can't be defined at the top-level */
package object data {
  type Hand = NonEmptyList[Card]
  type Deck = List[Card]
}
